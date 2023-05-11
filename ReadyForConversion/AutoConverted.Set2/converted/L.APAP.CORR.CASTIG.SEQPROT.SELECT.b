*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.CORR.CASTIG.SEQPROT.SELECT
*
* Client Name   : APAP
* Develop By    : Ashokkumar
* Description   : The routine to adjust the Insurance (SEGPROTFIN1) amount for the castigado prestamos.
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.ACCOUNT.DETAILS
    $INSERT I_L.APAP.CORR.CASTIG.SEQPROT.COMMON


    NO.RECS = ''; SEL.ERR = ''; ACCOUNT.LIST = ''
    SEL.CMD = "SELECT ":FN.AA.ACCOUNT.DETAILS:" WITH SUSPENDED EQ 'YES'"
    CALL EB.READLIST(SEL.CMD, ACCOUNT.LIST, '', NO.RECS, SEL.ERR)
    CALL BATCH.BUILD.LIST('',ACCOUNT.LIST)

    RETURN
END
