package FFT

import chisel3._
import chisel3.experimental._
import chisel3.util._
import scala.math._

class myFFTIO extends Bundle {
    val din = Input(new myComplex())
    val din_valid = Input(Bool())
    val dout0 = Output(new myComplex())
    val dout1 = Output(new myComplex())
    val dout_valid = Output(Bool())
}

class myFFT extends Module with HasDataConfig with HasElaborateConfig {
    val io = IO(new myFFTIO())

    def sinTable(stage:Int): Vec[FixedPoint] = {
        val sincoff = (0 until FFTlength / 2 by pow(2, stage - 1).toInt).map(
            i => (2 * Pi * i) / FFTlength.toDouble
        ).map(
            j => sin(-j).F(DataWidth.W, BinaryPoint.BP)
        )
        VecInit(sincoff)
    }

    def cosTable(stage:Int): Vec[FixedPoint] = {
        val coscoff = (0 until FFTlength / 2 by pow(2, stage - 1).toInt).map(
            i => (2 * Pi * i) / FFTlength.toDouble
        ).map(
            j => cos(-j).F(DataWidth.W, BinaryPoint.BP)
        )
        VecInit(coscoff)
    }

    def wnTable(stage: Int)(n: UInt): myComplex = {
        val res = Wire(new myComplex())
        res.re := sinTable(stage)(n)
        res.im := sinTable(stage)(n)
        res
    }

    val stages = log2Ceil(FFTlength)
    val cnt = RegInit(0.U((stages - 1).W))

    when (io.din_valid){
        cnt := cnt + 1.U
    }

    val upper = VecInit(Seq.fill(stages + 1)(0.S(DataWidth.W).asTypeOf(new myComplex())))
    upper(0) := io.din

    val lower = VecInit(Seq.fill(stages + 1)(0.S(DataWidth.W).asTypeOf(new myComplex())))
    lower(0) := io.din
    //pow函数是double类型的
    for (i <- 1 until stages) {
        val order = cnt(stages - i - 1, 0)
        val wn = wnTable(i)(order)
        val bf = Butterfly(ShiftRegister(upper(i - 1), pow(2, stages - i + 1).toInt), lower(i - 1), wn)
        val contrl = cnt(stages - i - 1)
        val sw = switch(bf._1, ShiftRegister(bf._2, pow(2, stages - i).toInt), contrl)
        upper(1) := sw._1
        lower(1) := sw._2
    }

    val upperlast = RegNext(upper(stages - 1))
    upper(stages) := Add(upperlast, lower(stages - 1))
    lower(stages) := Sub(upperlast, lower(stages - 1))

    val upperout = RegNext(upper(stages))
    val lowerout = RegNext(lower(stages))

    io.dout0 := upperout
    io.dout1 := lowerout
    io.dout_valid := ShiftRegister(io.din_valid, FFTlength)
}