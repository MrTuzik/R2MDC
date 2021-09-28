package FFT

trait HasDataConfig {
    val DataWidth = 16
    val BinaryPoint = 8
}

trait HasElaborateConfig {
    val FFTlength = 64
}