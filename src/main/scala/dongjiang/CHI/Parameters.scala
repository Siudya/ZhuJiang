package DONGJIANG.CHI

import chisel3._
import chisel3.util._

case class CHIBundleParameters(
                                nodeIdBits: Int,
                                addressBits: Int,
                                dataBits: Int,
                                dataCheck: Boolean,
                                txnidBits: Int,
                                dbidBits: Int,
                                snpHasTgtId: Boolean,
                                // TODO: has snoop
                              ) {
    require(nodeIdBits >= 7 && nodeIdBits <= 11)
    require(addressBits >= 44 && addressBits <= 52)
    require(isPow2(dataBits))
    require(dataBits == 128 || dataBits == 256 || dataBits == 512)
}

object CHIBundleParameters {
    def apply(
               nodeIdBits: Int = 7,
               addressBits: Int = 44,
               dataBits: Int = 256,
               dataCheck: Boolean = false,
               txnidBits: Int = 8,
               dbidBits: Int = 8,
               snpHasTgtId: Boolean = false,
             ): CHIBundleParameters = new CHIBundleParameters(
        nodeIdBits = nodeIdBits,
        addressBits = addressBits,
        dataBits = dataBits,
        dataCheck = dataCheck,
        txnidBits = txnidBits,
        dbidBits = dbidBits,
        snpHasTgtId = snpHasTgtId
    )
}
