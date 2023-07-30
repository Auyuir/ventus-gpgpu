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
    val missRspIn = Flipped(ValidIO(new SpeMSHRmissRspIn(NMshrEntry)))
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
  val entryStatusForRsp = Module(new getEntryStatusRsp(NMshrEntry))

  // ******     enum vec_mshr_status     ******
  val mshrStatus_st1_r = RegInit(0.U(1.W))
  /*AVAIL         000
  * FULL          001*/
  // ******      mshr::probe_spe    ******
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

  io.probeOut_st1.probeStatus := mshrStatus_st1_r

  //  ******     mshr::allocate_special     ******
  /*0:AVAIL 1:FULL*/
  io.missReq.ready := mshrStatus_st1_r === 0.U
  assert(!io.missReq.fire || (io.missReq.fire && !io.missRspIn.fire),"MSHR cant have Req & Rsp valid in same cycle, later the prior")

  when(io.missReq.fire){
    instrId_Access(entryStatus.io.next) := io.missReq.bits.instrId
  }

  io.probeOut_st1.a_source := entryStatus.io.next

  //  ******      mshr::special_arrange_core_rsp    ******
  entryStatusForRsp.io.valid_list := Reverse(Cat(entry_valid))
  // priority: missRspIn > missReq
  //assert(!io.missRspIn.fire || (io.missRspIn.fire && subentryStatus.io.used >= 1.U))
  //This version allow missRspIn fire when no subentry are left
  //如果后面发现missRspOut端口这一级不能取消，使用这段注释掉的代码

  io.missRspOut.bits.instrId := RegNext(io.missRspIn.bits.d_source)
  io.missRspOut.valid := RegNext(io.missRspIn.valid)

  //  ******     maintain entries    ******
  for (iofEn <- 0 until NMshrEntry){
    when(iofEn.asUInt === entryStatus.io.next && io.missReq.fire) {
      entry_valid(iofEn) := true.B
    }.elsewhen(iofEn.asUInt === io.missRspIn.bits.d_source && io.missRspIn.valid) {
      entry_valid(iofEn) := false.B
    }
  }
}
