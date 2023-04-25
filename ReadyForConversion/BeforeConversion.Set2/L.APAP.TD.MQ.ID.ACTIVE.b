*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.TD.MQ.ID.ACTIVE
*--------------------------------------------------------------------------------------------------------
*--------------------------------------------------------------------------------------------------------
*Description  : This is a ID routine to from RABBIT interface
*Linked With  : LATAM.CARD.ORDER
*In Parameter : N/A
*Out Parameter: N/A
*--------------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
*    Date            Who                  Reference               Description
*   ------         ------               -------------            -------------
* 15 06 2020    Arcadio Ruiz           Proyecto Digitalizacion   Initial Creation
*--------------------------------------------------------------------------------------------------------
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_GTS.COMMON
    $INSERT TAM.BP I_F.LATAM.CARD.ORDER
    $INSERT TAM.BP I_F.LATAM.CARD.CUSTOMER
    $INSERT TAM.BP I_LATAM.CARD.COMMON

    GOSUB INIT
    GOSUB PROCESS

    RETURN

INIT:

    ID = V$DISPLAY

    Y.CUSTOMER = FIELD(ID,".",1)
    Y.END.CARD = FIELD(ID,".",2)

    FN.CARD.CUSTOMER  = "F.LATAM.CARD.CUSTOMER"
    F.CARD.CUSTOMER  = ""
    CALL OPF(FN.CARD.CUSTOMER,F.CARD.CUSTOMER)

    RETURN

PROCESS:

    CALL F.READ(FN.CARD.CUSTOMER,Y.CUSTOMER,R.CARD.CUSTOMER,F.CARD.CUSTOMER,ERROR.CUSTOMER)

    Y.CARDS = R.CARD.CUSTOMER<APAP.DC.CARD.NO>
    Y.CARDS.COUNT = DCOUNT(Y.CARDS,@VM)

    FOR A=1 TO Y.CARDS.COUNT STEP 1

        Y.CARDS.CU = Y.CARDS<1,A>

        IF RIGHT(Y.CARDS.CU,4)EQ Y.END.CARD THEN
            COMI = Y.CARDS.CU
            RETURN
        END ELSE
            V$DISPLAY = ""
        END

    NEXT A

    RETURN

*--------------------------------------------------------------------------------------------------------
