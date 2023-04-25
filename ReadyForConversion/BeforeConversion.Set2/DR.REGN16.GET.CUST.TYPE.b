*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE DR.REGN16.GET.CUST.TYPE
*-------------------------------------------------------------------------
* Date              Author                    Description
* ==========        ====================      ============
* 21/08/2014          Ashokkumar            PACS00366332- Corrected the Industry value
*-------------------------------------------------------------------------

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.CUSTOMER
    $INSERT T24.BP I_F.INDUSTRY
    $INSERT TAM.BP I_F.REDO.CATEGORY.CIUU

    CUST.ID = COMI
    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    R.CUSTOMER = ''; CUSTOMER.ERR = ''
    CALL F.READ(FN.CUSTOMER,CUST.ID,R.CUSTOMER,F.CUSTOMER,CUSTOMER.ERR)
    CALL DR.REG.GET.CUST.TYPE(R.CUSTOMER,OUT.ARR)
    COMI = OUT.ARR<1>
    RETURN
END
