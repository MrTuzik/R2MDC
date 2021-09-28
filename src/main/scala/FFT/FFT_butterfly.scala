package FFT

import chisel3._
import chisel3.experimental._

class myComplex extends Bundle with HasDataConfig {
    val re = FixedPoint(DataWidth.W,  BinaryPoint.BP)
    val im = FixedPoint(DataWidth.W,  BinaryPoint.BP)
}

class ComplexIO extends Bundle {
    val in0 = Input(new myComplex())
    val in1 = Input(new myComplex())
    val res = Output(new myComplex())
}

class Add extends Module {
    val io = IO(new ComplexIO())
    io.res.re := io.in0.re + io.in1.re
    io.res.im := io.in0.im + io.in1.im
}

object Add {
    def apply(in0: myComplex, in1: myComplex): myComplex = {
        val inst = Module(new Add())
        inst.io.in0 := in0
        inst.io.in1 := in1
        inst.io.res
    }
}

class Sub extends Module {
    val io = IO(new ComplexIO())
    io.res.re := io.in0.re - io.in1.re
    io.res.im := io.in0.im - io.in1.im
}

object Sub {
    def apply(in0: myComplex, in1: myComplex): myComplex = {
        val inst = Module(new Sub())
        inst.io.in0 := in0
        inst.io.in1 := in1
        inst.io.res
    }
}

class Mul extends Module {
    val io = IO(new ComplexIO())
    //guass complex multiply
    val temp0 = io.in0.re * (io.in1.re + io.in1.im)
    val temp1 = io.in1.im * (io.in0.re + io.in0.im)
    val temp2 = io.in1.re * (io.in0.im - io.in0.re)
    io.res.re := temp0 - temp1 //io.in0.re * io.in1.re - io.in0.im * io.in1.im
    io.res.im := temp0 + temp2 //io.in0.re * io.in1.im + io.in0.im * io.in1.re
 }

 object Mul {
     def apply(in0: myComplex, in1: myComplex): myComplex = {
         val inst = Module(new Mul())
         inst.io.in0 := in0
         inst.io.in1 := in1
         inst.io.res
     }
 }

 class ButterflyIO extends Bundle {
     val in0 = Input(new myComplex())
     val in1 = Input(new myComplex())
     val wn = Input(new myComplex())
     val out0 = Output(new myComplex())
     val out1 = Output(new myComplex())
 }

 class Butterfly extends Module {
     val io = IO(new ButterflyIO())
     val addtemp = Add(io.in0, io.in1)
     val subtemp = Sub(io.in0, io.in1)
     val multemp = Mul(subtemp,io.wn)
     io.out0 := addtemp
     io.out1 := multemp
 }

 object Butterfly {
     def apply(in0: myComplex, in1: myComplex, wn: myComplex): (myComplex, myComplex) = {
         val inst = Module(new Butterfly())
         inst.io.in0 := in0
         inst.io.in1 := in1
         inst.io.wn := wn
         (inst.io.out0, inst.io.out1)
     }
 }

 class switchIO extends Bundle {
     val in0 = Input(new myComplex())
     val in1 = Input(new myComplex())
     val sel = Input(Bool())
     val out0 = Output(new myComplex())
     val out1 = Output(new myComplex())
 }

 class switch extends Module {
     val io = IO(new switchIO())
     io.out0 := Mux(io.sel, io.in1, io.in0)
     io.out1 := Mux(io.sel, io.in0, io.in1)
 }

 object switch{
     def apply(in0: myComplex, in1: myComplex, sel: Bool): (myComplex, myComplex) = {
         val inst = Module(new switch())
         inst.io.in0 := in0
         inst.io.in1 := in1
         inst.io.sel := sel
         (inst.io.out0, inst.io.out1)
     }
 }