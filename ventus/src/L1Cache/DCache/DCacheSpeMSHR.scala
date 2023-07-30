/*
 * Copyright (c) 2021-2022 International Innovation Center of Tsinghua University, Shanghai
 * Ventus is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 * See the Mulan PSL v2 for more details. */
package L1Cache.DCache

import chisel3._
import chisel3.util._

//abstract class MSHRBundle extends Bundle with L1CacheParameters

class SpeMSHRprobe extends Bundle {
  val isSC = Bool()
}
class SpeMSHRprobeOut(val NEntry:Int) extends Bundle {
  val probeStatus = UInt(1.W)
  val a_source = UInt(log2Up(NEntry).W)
}
class SpeMSHRmissReq(val WIdBits: Int) extends Bundle {
  val instrId = UInt(WIdBits.W)
}
class SpeMSHRmissRspIn(val NEntry: Int) extends Bundle {//Use this bundle when a block return from Lower cache
  val d_source = UInt(log2Up(NEntry).W)//d_source
}
class SpeMSHRmissRspOut(val WIdBits: Int) extends Bundle {
  val instrId = UInt(WIdBits.W)
}

class getEntryStatusReq(nEntry: Int) extends Module{
  val io = IO(new Bundle{
    val valid_list = Input(UInt(nEntry.W))
    val alm_full = Output(Bool())
    val full = Output(Bool())
    val next = Output(UInt(log2Up(nEntry).W))
    //val used = Output(UInt())
  })

  val used: UInt = PopCount(io.valid_list)
  io.alm_full := used === (nEntry.U-1.U)
  io.full := io.valid_list.andR
  io.next := VecInit(io.valid_list.asBools).indexWhere(_ === false.B)
}

class getEntryStatusRsp(nEntry: Int) extends Module{
  val io = IO(new Bundle{
    val valid_list = Input(UInt(nEntry.W))
    val next2cancel = Output(UInt(log2Up(nEntry).W))
    val used = Output(UInt((log2Up(nEntry)+1).W))
  })
  io.next2cancel := VecInit(io.valid_list.asBools).indexWhere(_ === true.B)
  io.used := PopCount(io.valid_list)

}


class SpeMSHR(val WIdBits: Int, val NMshrEntry:Int) extends Module{
  val io = IO(new Bundle{
    val probe = Flipped(ValidIO(new SpeMSHRprobe))
    val probeOut_st1 = Output(new SpeMSHRprobeOut(NMshrEntry))
    val missReq = Flipped(Decoupled(new SpeMSHRmissReq(WIdBits)))
    val missRspIn = Flipped(Decoupled(new SpeMSHRmissRspIn(NMshrEntry)))
    val missRspOut = ValidIO(new SpeMSHRmissRspOut(WIdBits))
    //For InOrFlu
    val empty = Output(Bool())
  })
  val instrId_Access = RegInit(VecInit(Seq.fill(NMshrEntry)(0.U(WIdBits.W))))

  val entry_valid = RegInit(VecInit(Seq.fill(NMshrEntry)(false.B)))
  io.empty := !entry_valid.asUInt.orR

  //  ******     missReq decide selected subentries are full or not     ******
  val entryStatus = Module(new getEntryStatusReq(NMshrEntry)) // Output: alm_full, full, next
  entryStatus.io.valid_list := Reverse(Cat(entry_valid))

  //  ******     missRsp status      ******
  val subentryStatusForRsp = Module(new getEntryStatusRsp(NMshrEntry))

  // ******     enum vec_mshr_status     ******
  val mshrStatus_st1_r = RegInit(0.U(3.W))
  val mshrStatus_st1_w = Wire(UInt(1.W))
  /*AVAIL         000
  * FULL          001*/
  // ******      mshr::probe_vec    ******
  val entryFull = entryStatus.io.full
  val entryAlmFull = entryStatus.io.alm_full
  when(io.missReq.fire){
    when(entryAlmFull) {
      mshrStatus_st1_r := 1.U //FULL
    }
  }.elsewhen(io.probe.valid){
    when(entryFull) {
      mshrStatus_st1_r := 1.U //FULL
    }.otherwise {
      mshrStatus_st1_r := 0.U //AVAIL
    }
  }.elsewhen(io.missRspIn.valid){
    when(mshrStatus_st1_r === 1.U){
      mshrStatus_st1_r := 0.U //AVAIL
    }
  }
  when(secondaryMiss && (mshrStatus_st1_r === 0.U || mshrStatus_st1_r === 1.U)){
    when(subEntryFull) {
      mshrStatus_st1_w := 3.U //SECONDARY_FULL
    }.otherwise {
      mshrStatus_st1_w := 2.U //SECONDARY_AVAIL
    }
  }.otherwise{
    mshrStatus_st1_w := mshrStatus_st1_r
  }
  io.probeOut_st1.probeStatus := mshrStatus_st1_w

  //  ******     mshr::allocate_vec_sub/allocate_vec_main     ******
  /*0:PRIMARY_AVAIL 1:PRIMARY_FULL 2:SECONDARY_AVAIL 3:SECONDARY_FULL*/
  io.missReq.ready := !(mshrStatus_st1_w === 1.U || mshrStatus_st1_w === 3.U)// || io.missRspIn.valid)
  assert(!io.missReq.fire || (io.missReq.fire && !io.missRspIn.fire),"MSHR cant have Req & Rsp valid in same cycle, later the prior")
  val real_SRAMAddrUp = Mux(secondaryMiss,OHToUInt(entryMatchProbe_st1),entryStatus.io.next)
  val real_SRAMAddrDown = Mux(secondaryMiss,entryStatus.io.next,0.U)
  when (io.missReq.fire){
    targetInfo_Accesss(real_SRAMAddrUp)(real_SRAMAddrDown) := io.missReq.bits.targetInfo
  }

  when(io.missReq.fire && mshrStatus_st1_w === 0.U) {//PRIMARY_AVAIL
    blockAddr_Access(entryStatus.io.next) := io.missReq.bits.blockAddr
    instrId_Access(entryStatus.io.next) := io.missReq.bits.instrId
  }

  io.probeOut_st1.a_source := real_SRAMAddrUp

  //  ******      mshr::vec_arrange_core_rsp    ******
  subentryStatusForRsp.io.valid_list := Reverse(Cat(subentry_valid(entryMatchMissRsp)))
  // priority: missRspIn > missReq
  //assert(!io.missRspIn.fire || (io.missRspIn.fire && subentryStatus.io.used >= 1.U))
  //This version allow missRspIn fire when no subentry are left
  //如果后面发现missRspOut端口这一级不能取消，使用这段注释掉的代码
  //io.missRspIn.ready := !(subentryStatusForRsp.io.used >= 2.U ||
  //  (subentryStatusForRsp.io.used === 1.U && !io.missRspOut.ready))
  io.missRspIn.ready := !((subentryStatusForRsp.io.used >= 2.U) ||
    ((mshrStatus_st1_w === 4.U || mshrStatus_st1_w === 3.U) && subentryStatusForRsp.io.used === 1.U))

  entryMatchMissRsp := io.missRspIn.bits.instrId
  //entryMatchMissRsp := Reverse(Cat(instrId_Access.map(_ === io.missRspIn.bits.instrId))) & entry_valid
  //assert(PopCount(entryMatchMissRsp) <= 1.U,"MSHR missRspIn, cant match multiple entries")
  val subentry_next2cancel = Wire(UInt(log2Up(NMshrSubEntry).W))
  subentry_next2cancel := subentryStatusForRsp.io.next2cancel

  io.missRspOut.bits.targetInfo := RegNext(missRspTargetInfo_st0)
  io.missRspOut.bits.blockAddr := RegNext(missRspBlockAddr_st0)
  io.missRspOut.bits.instrId := io.missRspIn.bits.instrId
  io.missRspOut.valid := RegNext(io.missRspIn.valid)
  //io.missRspOut := RegNext(io.missRspIn.valid) &&
  //  subentryStatusForRsp.io.used >= 1.U//如果上述Access中改出SRAM，本信号需要延迟一个周期

  //  ******     maintain subentries    ******
  /*0:PRIMARY_AVAIL 1:PRIMARY_FULL 2:SECONDARY_AVAIL 3:SECONDARY_FULL*/
  for (iofEn <- 0 until NMshrEntry){
    for (iofSubEn <- 0 until NMshrSubEntry){
      when(iofEn.asUInt===entryStatus.io.next &&
        iofSubEn.asUInt===0.U && io.missReq.fire && primaryMiss){
        subentry_valid(iofEn)(iofSubEn) := true.B
      }.elsewhen(iofEn.asUInt===entryMatchMissRsp){
        when(iofSubEn.asUInt===subentry_next2cancel &&
          io.missRspIn.valid){
          subentry_valid(iofEn)(iofSubEn) := false.B
        }.elsewhen(iofSubEn.asUInt===entryStatus.io.next &&
          io.missReq.fire && secondaryMiss){
          subentry_valid(iofEn)(iofSubEn) := true.B
        }
      }//order of when & elsewhen matters, as elsewhen cover some cases of when, but no op to them
    }
  }
}
