package OpenSoC

import Chisel._
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.util.Random
import Array._

class VCRouterState extends Chisel.Bundle {

    val idle            = UInt(0)
    val inputValid      = UInt(1)
    val packetRouted    = UInt(2)
    val vcAllocGranted  = UInt(3)
    val swAllocGranted  = UInt(4)
    val hold            = UInt(5)
    val packetComplete  = UInt(6)
}

class VCRouterOutputState extends Chisel.Bundle {
    
    val idle        = UInt(0)
    val rdyToXmit   = UInt(1)
    val xmit        = UInt(2)
    val hold        = UInt(3)

}

class VCRouterStateManagement(parms: Parameters) extends Module(parms){

    val io = new Bundle {
        val inputBufferValid    = Bool(INPUT) 
        val routingComplete     = Bool(INPUT) 
        val inputBufferIsTail   = Bool(INPUT) 
        val vcAllocGranted      = Bool(INPUT) 
        val swAllocGranted      = Bool(INPUT)
        val creditsAvail        = Bool(INPUT)
        val outputReady         = Bool(INPUT)

        val currentState         = UInt().asOutput 
        
    }

    val VCRouterState = new VCRouterState

    val curState = Reg(init = UInt(0))
    when(curState === VCRouterState.idle){
        when(io.inputBufferValid){
            curState := VCRouterState.inputValid
        }.otherwise {
            curState := VCRouterState.idle 
        }
    }.elsewhen(curState === VCRouterState.inputValid){
        when(io.routingComplete){
            curState := VCRouterState.packetRouted
        }.otherwise {
            curState := VCRouterState.inputValid
        }
    }.elsewhen(curState === VCRouterState.packetRouted){
        when(io.vcAllocGranted){
            curState := VCRouterState.vcAllocGranted
       } .otherwise {
            curState := VCRouterState.packetRouted
        }
    }.elsewhen(curState === VCRouterState.vcAllocGranted){
        when(io.swAllocGranted){
            curState := VCRouterState.swAllocGranted
        }.otherwise {
            curState := VCRouterState.vcAllocGranted
        }
    }.elsewhen(curState === VCRouterState.swAllocGranted){
        when(io.inputBufferIsTail && io.creditsAvail){
            curState := VCRouterState.idle
        }.elsewhen(io.creditsAvail){
            curState := VCRouterState.swAllocGranted
        }.otherwise{
            curState := VCRouterState.hold
        }
    }.elsewhen(curState === VCRouterState.hold){
        when(io.creditsAvail){
            curState := VCRouterState.swAllocGranted
        }.otherwise{
            curState := VCRouterState.hold
        }
    }


    io.currentState := curState 
}

class VCRouterOutputStateManagement(parms: Parameters) extends Module(parms){

    val io = new Bundle {
        val swAllocGranted  = Bool(INPUT)
        val creditsAvail    = Bool(INPUT)
        
        val currentState    = UInt().asOutput
    }

    val VCRouterOutputState       = new VCRouterOutputState

    val curState            = Reg(init = UInt(0))

    when(curState === VCRouterOutputState.idle){
        when(io.swAllocGranted && io.creditsAvail){
            curState := VCRouterOutputState.rdyToXmit
        }.otherwise {
            curState := VCRouterOutputState.idle
        }
    }.elsewhen(curState === VCRouterOutputState.rdyToXmit){
        curState := VCRouterOutputState.xmit
    }.elsewhen(curState === VCRouterOutputState.xmit){
        when(io.creditsAvail && io.swAllocGranted){
            curState := VCRouterOutputState.xmit
        }.elsewhen(~io.swAllocGranted){
            curState := VCRouterOutputState.idle
        }.otherwise {
            curState := VCRouterOutputState.hold
        }
    }.elsewhen(curState === VCRouterOutputState.hold){
        when(io.creditsAvail){
            curState := VCRouterOutputState.xmit
        }.otherwise {
            curState := VCRouterOutputState.hold
        }
    }

    io.currentState := curState

}
