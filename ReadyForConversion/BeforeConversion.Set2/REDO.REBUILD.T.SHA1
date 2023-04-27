*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* <Rating>-33</Rating>
*AZ.ACCOUNT id read from that application. And fetch the position of the local
*referance AZ.ACCOUNT id is read from AZ.ACCOUNT application and
*it fetches the position of local reference value L.AZ.SHA1.CODE . Using that get the sha1.code
*And make sha1.code as ID and store the corresponding account id in template

*
*-----------------------------------------------------------------------------
SUBROUTINE REDO.REBUILD.T.SHA1
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.CUSTOMER
$INSERT I_F.AZ.ACCOUNT
$INSERT I_F.REDO.T.SHA1
GOSUB INIT
GOSUB OPEN
GOSUB PROCESS

RETURN


******************
INIT:

SEL.CMD=''
AZ.SEL.LIST=''
AZ.ERR=''
AZ.ACCOUNT.ID=''
SEL.POS=0
R.CUS=''
ERR.SHA1=''
POS=0

AZ.NOR=0
ID.SHA1=''


R.CUS.SHA1=''
ERR.SHA2=''
RETURN
***********
OPEN:

FN.AZ.ACCOUNT='F.AZ.ACCOUNT'
F.AZ.ACCOUNT=''
CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

FN.REDO.T.SHA1 = 'F.REDO.T.SHA1'
F.REDO.T.SHA1 = ''
CALL OPF(FN.REDO.T.SHA1,F.REDO.T.SHA1)

L.AZ.SHA1.CODE.POS = ''
CALL GET.LOC.REF('AZ.ACCOUNT','L.AZ.SHA1.CODE',L.AZ.SHA1.CODE.POS)
RETURN
**********
PROCESS:

SEL.CMD="SELECT ":FN.AZ.ACCOUNT

CALL EB.READLIST(SEL.CMD,AZ.SEL.LIST,'',AZ.NOR,AZ.ERR)

LOOP

REMOVE AZ.ACCOUNT.ID FROM AZ.SEL.LIST SETTING SEL.POS

WHILE AZ.ACCOUNT.ID:SEL.POS

CALL F.READ(FN.AZ.ACCOUNT,AZ.ACCOUNT.ID,R.CUS.SHA1,F.AZ.ACCOUNT,ERR.SHA1)
IF R.CUS.SHA1 THEN
SHA1.CODE = R.CUS.SHA1<AZ.LOCAL.REF,L.AZ.SHA1.CODE.POS>
END
IF SHA1.CODE THEN
CALL F.READ(FN.REDO.T.SHA1,SHA1.CODE,R.SHA1.CODE,F.REDO.T.SHA1,SHA1.ERR)
IF SHA1.ERR THEN
*R.SHA1.CODE<1> =ACC.ID ; * Tus Start
R.SHA1.CODE<RE.T.SH.AZ.ACCOUNT.NO> =ACC.ID ; * Tus End
END
CALL F.WRITE(FN.REDO.T.SHA1,SHA1.CODE,R.SHA1.CODE)
END
REPEAT
RETURN
**************
END
