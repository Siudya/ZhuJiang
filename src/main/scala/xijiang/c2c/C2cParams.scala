package xijiang.c2c

case class C2cParams(
  reqBufTxDepth: Int = 8,
  respBufTxDepth: Int = 8,
  dataBufTxDepth: Int = 8,
  snoopBufTxDepth: Int = 8,

  reqBufRxDepth: Int = 2,
  respBufRxDepth: Int = 2,
  dataBufRxDepth: Int = 2,
  snoopBufRxDepth: Int = 2
)
