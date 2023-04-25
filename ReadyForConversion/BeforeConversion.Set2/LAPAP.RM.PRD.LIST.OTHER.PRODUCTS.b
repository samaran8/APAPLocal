*-----------------------------------------------------------------------------
* <Rating>-2</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.RM.PRD.LIST.OTHER.PRODUCTS

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.DATES
    $INSERT T24.BP I_F.CR.OTHER.PRODUCTS
    $INSERT TAM.BP I_F.REDO.CUST.PRD.LIST

    FN.PRD = "F.REDO.CUST.PRD.LIST"
    F.PRD = ""
    CALL OPF(FN.PRD,F.PRD)

    FN.OTHER = "F.CR.OTHER.PRODUCTS"
    F.OTHER = ""
    CALL OPF(FN.OTHER,F.OTHER)

    CUSTOMER = R.NEW(CR.OP.CUSTOMER) ;* 10000013 ;* 1000245
    PRODUCT.ID = ID.NEW

    CALL F.READ(FN.PRD,CUSTOMER,R.PRD,F.PRD,ERR.PRD)
    UU = R.PRD<PRD.PRODUCT.ID>

*-----------------------------------
*This function accept S,R,I,A etc...
*-----------------------------------
    IF V$FUNCTION EQ 'R' THEN

        M = DCOUNT(UU,@VM)
        FOR A = 1 TO M STEP 1
            IF R.PRD<PRD.PRODUCT.ID,A> EQ PRODUCT.ID  THEN

                R.PRD<PRD.PRD.STATUS,A> = "CLOSED"
                CALL F.WRITE(FN.PRD,CUSTOMER,R.PRD)
*               CALL JOURNAL.UPDATE('')

            END
        NEXT A

    END



END
