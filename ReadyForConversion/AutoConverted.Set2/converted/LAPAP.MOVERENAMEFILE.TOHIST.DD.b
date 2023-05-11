SUBROUTINE LAPAP.MOVERENAMEFILE.TOHIST.DD

***-------*******-------*******-------*******-------*******-------*******-------*********-------**********-------******

* Descripcion: Duplica archivo procesado con sobreescritura en nueva ruta, renombra y elimina archivo en ruta original.
* Atachado/Disponible en: Batch de Servicio BNK/REDO.VP.DD.SERVICE
* Requerimiento: CN009103

***-------*******-------*******-------*******-------*******-------*******-------*********-------**********-------******

    $INSERT I_F.DATES
    $INSERT I_F.REDO.VISION.PLUS.PARAM

* Open tables
    FN.REDO.VISION.PLUS.PARAM = "F.REDO.VISION.PLUS.PARAM"
    F.REDO.VISION.PLUS.PARAM= ""
    CALL OPF(FN.REDO.VISION.PLUS.PARAM,F.REDO.VISION.PLUS.PARAM)


    CALL CACHE.READ(FN.REDO.VISION.PLUS.PARAM, "SYSTEM", R.VER.ARR, VER.ARR.ERR)

* Get current date time in format YYYY.MM.DD.HHSS
    Y.DATE=OCONV(DATE(),"D-")
    Y.DATE=Y.DATE[9,2]:Y.DATE[1,2]:Y.DATE[4,2]
    Y.DATETIME.FORMAT=OCONV(ICONV(Y.DATE[1,6],"D2/"),'D4Y'):Y.DATE[3,4]:TIMEDATE()[1,2]:TIMEDATE()[4,2]
    Y.DD.FILENAME= R.VER.ARR<VP.PARAM.DD.FILE.NAME>
    Y.DD.FILEPATH= R.VER.ARR<VP.PARAM.DD.FILE.PATH>
    Y.DD.FILENAME.NEW.ID=Y.DD.FILENAME:".":Y.DATETIME.FORMAT

* Command that copy file, rename it with new ID and delete it from source
    Y.COMMAND = 'COPY FROM ':Y.DD.FILEPATH: ' TO ../interface/DD/DD_BACKUP ' :Y.DD.FILENAME: ',':Y.DD.FILENAME.NEW.ID : ' OVERWRITING DELETING'

    EXECUTE Y.COMMAND

RETURN

END
