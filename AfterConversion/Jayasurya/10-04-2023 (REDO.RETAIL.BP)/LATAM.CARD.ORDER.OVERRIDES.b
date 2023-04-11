* @ValidationCode : Mjo4MDYzMTE3NDU6Q3AxMjUyOjE2ODA3ODIzMDU5NTI6SVRTU0JORzotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 06 Apr 2023 17:28:25
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDORETAIL
SUBROUTINE LATAM.CARD.ORDER.OVERRIDES
*----------------------------------------------------------------------------
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : LATAM.CARD.ORDER.ID
*--------------------------------------------------------------------------------------------------------
*Description  : This is a ID routine to
*Linked With  : LATAM.CARD.ORDER
*In Parameter : N/A
*Out Parameter: N/A
*--------------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
*    Date            Who                  Reference               Description
*   ------         ------               -------------            -------------
* 9 Aug 2010    Mohammed Anies K       ODR-2010-03-0400          Initial Creation
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*06-04-2023            CONVERSION TOOL                AUTO R22 CODE CONVERSION           VM TO @VM ,FM TO @FM ,= TO EQ, #" TO NE
*06-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*--------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.DATES
    $INSERT I_F.LATAM.CARD.ORDER



    GOSUB OVERRIDES
RETURN
*-----OVERRIDES----------------------------------------------------------
*
OVERRIDES:
*********
    IF ID.OLD NE '' THEN
        RETURN
    END

    CURR.NO=0
    CALL STORE.OVERRIDE(CURR.NO)          ;* Initialising
    CHARGE.DATE = R.NEW(CARD.IS.CHARGE.DATE)
    GOSUB PROCESS.OVERRIDES

    IF TEXT EQ 'NO' THEN
        V$ERROR=1
        MESSAGE = "ERRROR"
    END

RETURN
******************************************************
PROCESS.OVERRIDES:
***************
    CURR.NO = DCOUNT(R.NEW(CARD.IS.OVERRIDE),@VM)+1
    IF CHARGE.DATE NE '' THEN ;* AUTO R22 CODE CONVERSION
        BEGIN CASE
            CASE CHARGE.DATE GT R.DATES(EB.DAT.FORW.VALUE.MINIMUM)
                TEXT='FWD.VAL.DATE'
                AF=CARD.IS.CHARGE.DATE
                CALL STORE.OVERRIDE(CURR.NO)
            CASE CHARGE.DATE LT R.DATES(EB.DAT.BACK.VALUE.MINIMUM)
                TEXT='BCK.VAL.DATE'
                AF=CARD.IS.CHARGE.DATE
                CALL STORE.OVERRIDE(CURR.NO)
        END CASE

        DAYTYPE=''
        CALL AWD('',CHARGE.DATE,DAYTYPE)
        IF DAYTYPE EQ 'H' THEN ;* AUTO R22 CODE CONVERSION
            TEXT='CRG.DATE.N.WRK'
            AF=CARD.IS.CHARGE.DATE
            CALL STORE.OVERRIDE(CURR.NO)
        END
    END
RETURN
*------------------------------------------------------------
END
