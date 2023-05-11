* @ValidationCode : MjotMTQ1MjQ3Njg3MzpDcDEyNTI6MTY4MTM3Nzc0NDczNDpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 13 Apr 2023 14:52:24
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
SUBROUTINE REDO.DS.AZ.INT.RATE(Y.INT.RATE)
*-----------------------------------------------------------------------------
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : REDO.DS.AZ.INT.RATE
*------------------------------------------------------------------------------
*Description  : This is a conversion routine used to display deposit amount
*               in words
*Linked With  :
*In Parameter : NA
*Out Parameter: Y.INT.RATE
*-------------------------------------------------------------------------------
* Modification History :
*-----------------------
*    Date            Who                  Reference               Description
*   ------          ------               -------------            -------------
* 10.11.2011      SUDHARSANAN S           CR.18                  INITIAL CREATION
* 28/10/2014      Vignesh Kumaar R        PACS00409994           MIS-SPELLED INTEREST RATE IN SPANISH WORDINGS
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*--------------------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT

*--------------------------------------------------------------------------------
**********
MAIN.PARA:
**********
    GOSUB PROCESS.PARA
RETURN
*--------------------------------------------------------------------------------
*************
PROCESS.PARA:
*************
    INT.RATE=R.NEW(AZ.INTEREST.RATE)
    INT.RATE1=FIELD(INT.RATE,'.',1)
    INT.RATE2=FIELD(INT.RATE,'.',2)
    INT.RATE2 = FMT(INT.RATE2,"L%2")
    Y.INT.RATE.CHECK = INT.RATE2[1,1]     ;* Added for PACS00409994
    VAR.INT.RATE = INT.RATE1:'.':INT.RATE2
    OUT.RATE1=''  ; OUT.RATE2=''
    LANGUAGE='ES'
    LINE.LENGTH=100
    NO.OF.LINES=1
    ERR.MSG=''
    CALL DE.O.PRINT.WORDS(INT.RATE1,OUT.RATE1,LANGUAGE,LINE.LENGTH,NO.OF.LINES,ERR.MSG)
    CHANGE '*' TO ' ' IN OUT.RATE1
    IF NOT(INT.RATE2) THEN
        Y.INT.RATE = VAR.INT.RATE:"% (":TRIM(OUT.RATE1,"",E):" POR CIENTO)"
    END ELSE
        CALL DE.O.PRINT.WORDS(INT.RATE2,OUT.RATE2,LANGUAGE,LINE.LENGTH,NO.OF.LINES,ERR.MSG)
        CHANGE '*' TO ' ' IN OUT.RATE2

* Fix for PACS00409994 [MIS-SPELLED INTEREST RATE IN SPANISH WORDINGS]

        IF Y.INT.RATE.CHECK EQ 0 THEN
            Y.INT.RATE= VAR.INT.RATE:"% (":TRIM(OUT.RATE1,"",E):" PUNTO CERO ":TRIM(OUT.RATE2,"",E):" POR CIENTO)"
        END ELSE

* End of Fix
            Y.INT.RATE= VAR.INT.RATE:"% (":TRIM(OUT.RATE1,"",E):" PUNTO ":TRIM(OUT.RATE2,"",E):" POR CIENTO)"
        END
    END
RETURN
*-------------------------------------------------------------------------------------
END
