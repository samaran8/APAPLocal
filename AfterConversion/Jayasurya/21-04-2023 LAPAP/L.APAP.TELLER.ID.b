* @ValidationCode : MjoxMTcyNDQ1ODQ6Q3AxMjUyOjE2ODIwNjg4NTI2MTc6SVRTU0JORzotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 21 Apr 2023 14:50:52
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.LAPAP
SUBROUTINE L.APAP.TELLER.ID

* Item ID        : GDC-292
*-------------------------------------------------------------------------------------
* Description :
* ------------
* This program allow to get the last curr.no record from TELLER.ID$HIS based upon different conditions.
*-------------------------------------------------------------------------------------
* Modification History :
* ----------------------
* Date           Author            Modification Description
* -------------  -----------       ---------------------------
* 2019/04/08     Raquel P.S.         Initial development
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*21-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 = TO EQ, < TO LT
*21-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*-------------------------------------------------------------------------------------
* Content summary :
* -----------------
* Enquiry : LAPAP.ENQ.RPT.LAB.CAJEROS
* EB record      : L.APAP.TELLER.ID
*-------------------------------------------------------------------------------------


    $INSERT I_EQUATE
    $INSERT I_COMMON
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.TELLER.ID

    FN.TT.ID.HIS = 'F.TELLER.ID$HIS'
    F.TT.ID.HIS = ''
    CALL OPF(FN.TT.ID.HIS, F.TT.ID.HIS)

    FN.TT.ID = 'F.TELLER.ID'
    F.TT.ID = ''
    CALL OPF(FN.TT.ID, F.TT.ID)

    Y.INPUT.ID = O.DATA
    Y.TELLER.ID= LEFT(Y.INPUT.ID,4)
    Y.MAX = ""

*****************************************************************************
*Getting Date and branch value
******************************************************************************

    Y.ENQ.SEL.BRANCH=ENQ.SELECTION<4,1>

    IF INDEX(Y.ENQ.SEL.BRANCH,"DO",1) EQ 0 THEN ;* AUTO R22 CODE CONVERSION = TO EQ
        Y.ENQ.SEL.BRANCH=ENQ.SELECTION<4,2>
        Y.ENQ.SEL.DATEOC=ENQ.SELECTION<4,1>
    END ELSE
        Y.ENQ.SEL.DATEOC=ENQ.SELECTION<4,2>
    END

******************************************************************************
*To get the maximum curr.no, it can not be used the Method F.READ.HISTORY
*since it is required to get the highest  value, but within several conditions
******************************************************************************

    IF Y.ENQ.SEL.BRANCH NE ""  THEN
        SEL.CMD = "SELECT " :FN.TT.ID.HIS : " WITH @ID LIKE " : Y.TELLER.ID :"... AND CO.CODE EQ ": Y.ENQ.SEL.BRANCH : " AND DATE.OF.CLOSE EQ ": Y.ENQ.SEL.DATEOC  :" AND L.TT.USER.TYPE EQ TELLER"
    END ELSE
        SEL.CMD = "SELECT " :FN.TT.ID.HIS : " WITH @ID LIKE " : Y.TELLER.ID :"... AND DATE.OF.CLOSE EQ ": Y.ENQ.SEL.DATEOC  :" AND L.TT.USER.TYPE EQ TELLER"
    END
    CALL EB.READLIST(SEL.CMD, SEL.LIST.GRAL,"", NO.OF.REC.GRAL, SEL.ERR)

    LOOP
        REMOVE RTE.REP.ID FROM SEL.LIST.GRAL SETTING RTE.POS
    WHILE RTE.REP.ID:RTE.POS

        ID.LENGHT=LEN(RTE.REP.ID)
        COMMA.POS=INDEX(RTE.REP.ID, ";",1)
        CURR.FROM.ID=SUBSTRINGS(RTE.REP.ID,COMMA.POS+1,ID.LENGHT)

        IF Y.MAX EQ "" THEN
            Y.MAX=CURR.FROM.ID
        END ELSE
            IF Y.MAX LT CURR.FROM.ID THEN ;* AUTO R22 CODE CONVERSION < TO LT
                Y.MAX=Y.MAX
            END ELSE
                Y.MAX=CURR.FROM.ID
            END
        END

    REPEAT

    O.DATA=Y.TELLER.ID:";":Y.MAX
*    O.DATA = "Cmd ": SEL.CMD
RETURN

END
