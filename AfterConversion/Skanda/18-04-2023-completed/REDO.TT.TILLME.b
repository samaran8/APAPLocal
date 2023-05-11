$PACKAGE APAP.TAM
SUBROUTINE REDO.TT.TILLME(V.DR.DENOMINATIO)
*--------------------------------------------------------------------------------
*Company Name :Asociacion Popular de Ahorros y Prestamos
*Developed By :btorresalbornoz
*Program Name :REDO.TT.TILLTOTILL DS
*---------------------------------------------------------------------------------
*DESCRIPTION :This program is used to get the SELL DESTINATION value from EB.LOOKUP TABLE
* ----------------------------------------------------------------------------------
* Modification History :
*-----------------------
* Date:                        Reference           Modified By:                Modification History
*-----------------------------------------------------------------------------
* 04-04-2023    TSR-452565     VIJAYALAKSHMI S          Multiple problems present in the alignment of deal slips produced by teller versions
** 18-04-2023 R22 Auto Conversion - FM TO @FM, VM to @VM, SM to @SM
** 18-04-2023 Skanda R22 Manual Conversion - No changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER
    $INSERT I_F.TELLER.ID
    $INSERT I_F.USER
    $INSERT I_F.TELLER.DENOMINATION
    $INSERT I_F.CURRENCY
    $INSERT I_F.COMPANY
    $INSERT I_F.ACCOUNT
    GOSUB INITIALISE
    GOSUB DENOMINATION
    GOSUB PROCESS
RETURN

* =========
INITIALISE:
* =========
*

    FN.TELLER.ID = 'F.TELLER.ID'
    F.TELLER.ID = ''
    CALL OPF(FN.TELLER.ID,F.TELLER.ID)

    FN.CURRENCY = 'F.CURRENCY'
    F.CURRENCY = ''
    CALL OPF(FN.CURRENCY,F.CURRENCY)


    FN.TELLER.DENOM = 'F.TELLER.DENOMINATION'
    F.TELLER.DENOM = ''
    CALL OPF(FN.TELLER.DENOM,F.TELLER.DENOM)

    V.DR.DENOMINATIO=''
    Y.VAR2 = 1
    Y.DENOM.NUM = 0
    W.DENOM = R.NEW(TT.TE.DENOMINATION)
    Y.DENOM.NUM = DCOUNT(W.DENOM,@VM)
    W.UNIT = R.NEW(TT.TE.DR.UNIT)
    Y.DENOM.UNIT = DCOUNT(W.UNIT,@VM)

    Y.VAR1 = 1
    Y.VAR.FIJA=1

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    LOC.REF.FIELD = 'L.TT.CONCEPT'
    LOC.REF.APP = 'TELLER'
    LOC.POS = ''
    CALL GET.LOC.REF(LOC.REF.APP,LOC.REF.FIELD,LOC.POS)




RETURN



*********
PROCESS:
*********


    DATA.FIJA=12


    LOOP

    WHILE Y.VAR.FIJA LE DATA.FIJA
        GOSUB ESTRUCTURA


        GOSUB SEGUNDA.ESTRUCTURA
        Y.VAR.FIJA += 1 ;* R22 Auto conversion

    REPEAT

RETURN

********************************************************************************
ESTRUCTURA:
********************************************************************************

    BEGIN CASE

        CASE Y.VAR.FIJA EQ '1'
            PART1= "Referencia:":SPACE(24):ID.NEW
            V.DR.DENOMINATIO<1,Y.VAR.FIJA>=PART1:SPACE(6):V.DR.DENOMINATIO<1,Y.VAR.FIJA>

        CASE Y.VAR.FIJA EQ '2' ;* R22 Auto conversion
            GOSUB TELLER.ID.USER.2
            PART2="De Cajero(Crédito):":SPACE(16):R.NEW(TT.TE.TELLER.ID.2):"/":Y.USER.2[1,7]
            V.DR.DENOMINATIO<1,Y.VAR.FIJA>=PART2:SPACE(6):V.DR.DENOMINATIO<1,Y.VAR.FIJA>


        CASE Y.VAR.FIJA EQ '3'
            GOSUB TELLER.ID.USER.1
            PART3= "A Cajero(Débito):":SPACE(18):R.NEW(TT.TE.TELLER.ID.1):"/":Y.USER.1[1,7]
            V.DR.DENOMINATIO<1,Y.VAR.FIJA>=PART3:SPACE(6):V.DR.DENOMINATIO<1,Y.VAR.FIJA>


        CASE Y.VAR.FIJA EQ '4'
            GOSUB CURRENCY.NAME
            Y.CURRENCY=R.CURRENCY<EB.CUR.CCY.NAME>
            PART4="Moneda:":SPACE(30):Y.CURRENCY
            V.DR.DENOMINATIO<1,Y.VAR.FIJA>=PART4:SPACE(6):V.DR.DENOMINATIO<1,Y.VAR.FIJA>



        CASE Y.VAR.FIJA EQ '5'

            Y.MONTO=R.NEW(TT.TE.AMOUNT.FCY.1)
            Y.MONTO=FMT(Y.MONTO,"R2,#16")
            PART5="Monto :":SPACE(24):Y.MONTO
            V.DR.DENOMINATIO<1,Y.VAR.FIJA>=PART5:SPACE(6):V.DR.DENOMINATIO<1,Y.VAR.FIJA>



        CASE Y.VAR.FIJA EQ '6' ;* R22 Auto conversion
            V.DR.DENOMINATIO<1,Y.VAR.FIJA>=SPACE(53):V.DR.DENOMINATIO<1,Y.VAR.FIJA>




        CASE Y.VAR.FIJA EQ '7'
            GET.DATE.TIME = R.NEW(TT.TE.DATE.TIME)
            GOSUB GET.DATE.TIME.INFO
            PART7="Fecha - Hora:":SPACE(19):GET.DATE.TIME
            V.DR.DENOMINATIO<1,Y.VAR.FIJA>=PART7:SPACE(6):V.DR.DENOMINATIO<1,Y.VAR.FIJA>



    END CASE

RETURN

********************************************************************************
SEGUNDA.ESTRUCTURA:
********************************************************************************

    BEGIN CASE
        CASE Y.VAR.FIJA EQ '8'
            GET.CO.CODE ='APAP':" ":R.COMPANY(EB.COM.COMPANY.NAME):"-":R.NEW(TT.TE.TELLER.ID.2)
            GET.CO.CODE = FMT(GET.CO.CODE,"28R")
            PART8="Oficina - Cajero: ":SPACE(1):GET.CO.CODE
            V.DR.DENOMINATIO<1,Y.VAR.FIJA>=PART8:SPACE(6):V.DR.DENOMINATIO<1,Y.VAR.FIJA>


        CASE Y.VAR.FIJA EQ '9'
            Y.COMENTARIO = R.NEW(TT.TE.NARRATIVE.2)
            Y.COMENTARIO=FMT(Y.COMENTARIO,"R#32")

            PART9="Comentarios:":SPACE(3):Y.COMENTARIO
            V.DR.DENOMINATIO<1,Y.VAR.FIJA>=PART9:SPACE(6):V.DR.DENOMINATIO<1,Y.VAR.FIJA>



        CASE Y.VAR.FIJA EQ '10'

            IF R.NEW(TT.TE.UNIT)<1,Y.VAR.FIJA> NE "0" THEN
                Y.DENOMINATIO=V.DR.DENOMINATIO<1,Y.VAR.FIJA>

                V.DR.DENOMINATIO<1,Y.VAR.FIJA>=" ":SPACE(52):Y.DENOMINATIO
            END

        CASE Y.VAR.FIJA EQ '11' ;* R22 Auto conversion
            IF R.NEW(TT.TE.UNIT)<1,Y.VAR.FIJA> NE "0" THEN
                V.DR.DENOMINATIO<1,Y.VAR.FIJA>=" ":SPACE(52):V.DR.DENOMINATIO<1,Y.VAR.FIJA>
            END

        CASE Y.VAR.FIJA EQ '12' ;* R22 Auto conversion
            IF R.NEW(TT.TE.UNIT)<1,Y.VAR.FIJA> NE "0" THEN
                V.DR.DENOMINATIO<1,Y.VAR.FIJA>=" ":SPACE(52):V.DR.DENOMINATIO<1,Y.VAR.FIJA>
            END

        CASE Y.VAR.FIJA EQ '13'
            IF R.NEW(TT.TE.UNIT)<1,Y.VAR.FIJA> NE "0" THEN
                V.DR.DENOMINATIO<1,Y.VAR.FIJA>=" ":SPACE(52):V.DR.DENOMINATIO<1,Y.VAR.FIJA>
            END


    END CASE

RETURN





*
********************************************************
TELLER.ID.USER.1:
*******************************************************

    Y.TELLER.ID1=R.NEW(TT.TE.TELLER.ID.1)

    CALL F.READ(FN.TELLER.ID,Y.TELLER.ID1,R.TELLER.ID,F.TELLER.ID,I.ERR)

    Y.USER.1=R.TELLER.ID<TT.TID.USER>


RETURN
********************************************************
TELLER.ID.USER.2:
*******************************************************

    Y.TELLER.ID2=R.NEW(TT.TE.TELLER.ID.2)

    CALL F.READ(FN.TELLER.ID,Y.TELLER.ID2,R.TELLER.ID,F.TELLER.ID,I.ERR)

    Y.USER.2=R.TELLER.ID<TT.TID.USER>


RETURN

**********************************************************
CURRENCY.NAME:
*********************************************************
    Y.CURRENCY=R.NEW(TT.TE.CURRENCY.1)
    CALL CACHE.READ(FN.CURRENCY, Y.CURRENCY, R.CURRENCY, Y.ERR) ;* R22 Auto conversion
    Y.CURRENCY=R.CURRENCY<EB.CUR.CCY.NAME>

RETURN

**********************************************************
DENOMINATION:
**********************************************************
    Y.DENOMINATION=R.NEW(TT.TE.DENOMINATION)<1,Y.VAR1>
    Y.UNIT=R.NEW(TT.TE.DR.UNIT)<1,Y.VAR1>

    LOOP
    WHILE Y.VAR1 LE Y.DENOM.NUM

        IF R.NEW(TT.TE.UNIT)<1,Y.VAR1> NE "0" THEN


            GOSUB GET.VALUE
            Y.DENOMINATION=R.NEW(TT.TE.DENOMINATION)<1,Y.VAR1>
            Y.DENOMINATION=FMT(Y.DENOMINATION,"11L")
            V.DR.DENOMINATIO<-1>=Y.DENOMINATION
            V.DR.DENOMINATIO= CHANGE(V.DR.DENOMINATIO,@FM,@VM)
*V.DR.DENOMINATIO=V.DR.DENOMINATIO:SPACE(10):R.NEW(TT.TE.UNIT)<1,Y.VAR1>:SPACE(3):V.DR.VALUE
*TSR-452565
            V.DR.UN = FMT(R.NEW(TT.TE.UNIT)<1,Y.VAR1>,"7R")
            V.DR.DENOMINATIO=V.DR.DENOMINATIO:SPACE(7):V.DR.UN:V.DR.VALUE
        END

        Y.VAR1 += 1 ;* R22 Auto conversion
    REPEAT



RETURN

*----------------------------------------------
GET.DATE.TIME.INFO:
*----------------------------------------------

    F1 = GET.DATE.TIME[1,2]
    F2 = GET.DATE.TIME[3,2]
    F3 = GET.DATE.TIME[5,2]
    F4 = GET.DATE.TIME[7,2]
    F5 = GET.DATE.TIME[9,2]

    Y.TIME = F3:'/':F2:'/':F1:'-':F4:':':F5
    GET.DATE.TIME= FMT(Y.TIME,"15R")

RETURN

*************************************************
GET.VALUE:
*************************************************
    Y.DENOM=R.NEW(TT.TE.DENOMINATION)<1,Y.VAR1>
    Y.UNIT=R.NEW(TT.TE.UNIT)<1,Y.VAR1>
    CALL CACHE.READ(FN.TELLER.DENOM,Y.DENOM,R.TELLER.DENOM,DENOM.ERR)
    DEN.VALUE = R.TELLER.DENOM<TT.DEN.VALUE>
*TSR-452565
    Y.UNIT = FMT(Y.UNIT,"6R")
    Y.VALUE = Y.UNIT * DEN.VALUE
    V.DR.VALUE=FMT(Y.VALUE,"R2,#19")


RETURN



********************************************************
ACCOUNT.NAME:
*******************************************************


    Y.ACCOUNT=R.NEW(TT.TE.ACCOUNT.1)

    CALL F.READ(FN.ACCOUNT,Y.ACCOUNT,R.ACCOUNT,F.ACCOUNT,IA.ERR)

    Y.ACCOUNT.NAME =R.ACCOUNT<AC.SHORT.TITLE>
    Y.ACCOUNT.NAME= Y.ACCOUNT.NAME[1,31]
    Y.ACCOUNT.NAME=FMT(Y.ACCOUNT.NAME,"R#32")
RETURN


END
