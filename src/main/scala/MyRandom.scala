trait Random { //interface Random
    def nextInt(x: Int): (Int, MyRandom)
}

case class MyRandom(seed: Long) extends Random { //o seed é uma espécie de semente que vai influenciar os números aleatórios que vão ser lançados
    def nextInt(x: Int): (Int, MyRandom) = { // x é o max que o número aleatório pode chegar, e o Myrandom garante que o próximo número sorteado será diferente e que a sequência de números continue parecendo aleatória
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRandom = MyRandom(newSeed) //criamos o novo random com base na nova sement
        val n = (newSeed >>> 16).toInt % x
        (if (n < 0) -n else n, nextRandom) // in case 'n' is negative, return it as positive
    }
}
