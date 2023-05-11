*------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* <Rating>-52</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.CUSTOMER.IDENT(customer,IDENT,IDENTYPE,NAME,LASTN,DEFV)
*------------------------------------------------------------------------------------
* Technical report:
* -----------------
* Company Name   : APAP
* Program Name   : LAPAP.CUSTOMER.IDENT
* Date           : 2018-03-15
* Item ID        : CN00
*------------------------------------------------------------------------------------
* Description :
* ------------
* This program runs through customer table looking for some identification number
*------------------------------------------------------------------------------------
* Modification History :
* ----------------------
* Date           Author            Modification Description
* -------------  -----------       ---------------------------
* 2018-03-15     Richard HC           Initial Development
* 2018-05-03     Richard HC           Functionalities inprovements
* 2022-02-18     APAP                Corregir logica del campo pasaporte , mapeando al campo LEGAL.ID
*------------------------------------------------------------------------------------
* Content summary :
* -----------------
* Table name     : CUSTOMER
* Auto Increment : N/A
* Views/versions : ALL VERSION TO REQUIRE IT
* EB record      : LAPAP.CUSTOMER.IDENT
* Routine        : LAPAP.CUSTOMER.IDENT
*------------------------------------------------------------------------------------

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.CUSTOMER
    $INSERT T24.BP I_F.ACCOUNT


    IF customer # '' THEN
        GOSUB INIT
        GOSUB INITb
        GOSUB PROCESS
        GOSUB END_PROCESS

    END ELSE
        GOSUB INIT
        GOSUB INIT_PROCESS
        GOSUB PROCESS
        GOSUB END_PROCESS
    END


INIT:
*----
    DEFV = "YES"
    FN.CUS = "F.CUSTOMER"
    F.CUS = ""
    FN.ACC = "F.ACCOUNT"
    F.ACC = ""
    ACC.ID = COMI   ;*1016568231       ;*COMI
    CALL OPF(FN.CUS,F.CUS)
    CALL OPF(FN.ACC,F.ACC)

    RETURN

INITb:
*----
    CALL F.READ(FN.CUS,customer,R.CUS,F.CUS,CUS.ERR)
    NAME = R.CUS<EB.CUS.GIVEN.NAMES>
    LASTN = R.CUS<EB.CUS.FAMILY.NAME>

    RETURN


    INIT_PROCESS:
*-------
!Getting customer id througt account number from account
    CALL F.READ(FN.ACC,ACC.ID,R.ACC,F.ACC,ACC.ERR)
    CUS.ID = R.ACC<AC.CUSTOMER>

    CALL F.READ(FN.CUS,CUS.ID,R.CUS,F.CUS,CUS.ERR)

    RETURN


PROCESS:
*-------
!Using customer id to fetch the correct iidentificacion
    CALL GET.LOC.REF("CUSTOMER","L.CU.CIDENT",POS)
    CARD.ID = R.CUS<EB.CUS.LOCAL.REF,POS>

    CALL GET.LOC.REF("CUSTOMER","L.CU.NOUNICO",POS)
    NO.UNIC = R.CUS<EB.CUS.LOCAL.REF,POS>

    CALL GET.LOC.REF("CUSTOMER","L.CU.ACTANAC",POS)
    B.CERT = R.CUS<EB.CUS.LOCAL.REF,POS>

    CALL GET.LOC.REF("CUSTOMER","L.CU.PASS.NAT",POS)
    PASS.N = R.CUS<EB.CUS.LOCAL.REF,POS>

    IF PASS.N EQ '' THEN
        PASS.N = R.CUS<EB.CUS.LEGAL.ID>
    END

    CALL GET.LOC.REF("CUSTOMER","L.CU.RNC",POS)
    RNC.NO = R.CUS<EB.CUS.LOCAL.REF,POS>



    RETURN


    END_PROCESS:
*---------------

    BEGIN CASE
    CASE CARD.ID NE ''
        COMI = CARD.ID
        IDENT = CARD.ID
        IDENTYPE = "CED"

    CASE NO.UNIC NE ''
        COMI = NO.UNIC
        IDENT = NO.UNIC
        IDENTYPE = "NUM.UNICO"

    CASE B.CERT NE ''
        COMI = B.CERT
        IDENT = B.CERT
        IDENTYPE = "CERT"

    CASE PASS.N NE ''
        COMI = PASS.N
        IDENT = PASS.N
        IDENTYPE = "PASS"


    CASE RNC.NO NE ''
        COMI = RNC.NO
        IDENT = RNC.NO
        IDENTYPE = "RNC"



    END CASE

    RETURN

END
