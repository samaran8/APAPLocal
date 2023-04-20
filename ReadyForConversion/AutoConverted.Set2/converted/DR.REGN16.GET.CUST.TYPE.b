SUBROUTINE DR.REGN16.GET.CUST.TYPE
*-------------------------------------------------------------------------
* Date              Author                    Description
* ==========        ====================      ============
* 21/08/2014          Ashokkumar            PACS00366332- Corrected the Industry value
*-------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.INDUSTRY
    $INSERT I_F.REDO.CATEGORY.CIUU

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
