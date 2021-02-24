      *>************************************************************************
      *>  SAMPLE DB2 CONNECTION AND VERIFICATION PROGRAM
      *>************************************************************************

      *>************************************************************************
      *> Program:      DB2CONN.sqb
      *>
      *> Purpose:      DB2 sample module
      *>
      *> Author:       AVINASH KUMAR
      *>
      *> Date-Written: 2021.02.15
      *>
      *>
      *>               Implemented features:
      *>               - connect to DB2
      *>               - FETCH CURRENT DATE
      *>               - connect reset
      *>
      *>************************************************************************
      *> Date       Name / Change description
      *> ========== ============================================================
      *> 2021.02.21 Avinash Kumar
      *>            - FIRST VERSION.
      *>************************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. DB2CONN.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  SQLDA-ID pic 9(4) comp-5.
       01  SQLDSIZE pic 9(4) comp-5.
       01  SQL-STMT-ID pic 9(4) comp-5.
       01  SQLVAR-INDEX pic 9(4) comp-5.
       01  SQL-DATA-TYPE pic 9(4) comp-5.
       01  SQL-HOST-VAR-LENGTH pic 9(9) comp-5.
       01  SQL-S-HOST-VAR-LENGTH pic 9(9) comp-5.
       01  SQL-S-LITERAL pic X(258).
       01  SQL-LITERAL1 pic X(130).
       01  SQL-LITERAL2 pic X(130).
       01  SQL-LITERAL3 pic X(130).
       01  SQL-LITERAL4 pic X(130).
       01  SQL-LITERAL5 pic X(130).
       01  SQL-LITERAL6 pic X(130).
       01  SQL-LITERAL7 pic X(130).
       01  SQL-LITERAL8 pic X(130).
       01  SQL-LITERAL9 pic X(130).
       01  SQL-LITERAL10 pic X(130).
       01  SQL-IS-LITERAL pic 9(4) comp-5 value 1.
       01  SQL-IS-INPUT-HVAR pic 9(4) comp-5 value 2.
       01  SQL-CALL-TYPE pic 9(4) comp-5.
       01  SQL-SECTIONUMBER pic 9(4) comp-5.
       01  SQL-INPUT-SQLDA-ID pic 9(4) comp-5.
       01  SQL-OUTPUT-SQLDA-ID pic 9(4) comp-5.
       01  SQL-VERSION-NUMBER pic 9(4) comp-5.
       01  SQL-ARRAY-SIZE pic 9(4) comp-5.
       01  SQL-IS-STRUCT  pic 9(4) comp-5.
       01  SQL-IS-IND-STRUCT pic 9(4) comp-5.
       01  SQL-STRUCT-SIZE pic 9(4) comp-5.
       01  SQLA-PROGRAM-ID.
           05 SQL-PART1 pic 9(4) COMP-5 value 172.
           05 SQL-PART2 pic X(6) value "AEAVAI".
           05 SQL-PART3 pic X(24) value "kBikNYCl01111 2         ".
           05 SQL-PART4 pic 9(4) COMP-5 value 8.
           05 SQL-PART5 pic X(8) value "DB2INST1".
           05 SQL-PART6 pic X(120) value LOW-VALUES.
           05 SQL-PART7 pic 9(4) COMP-5 value 8.
           05 SQL-PART8 pic X(8) value "DB2CONN ".
           05 SQL-PART9 pic X(120) value LOW-VALUES.
                               

      *> SQL communication area
       COPY "sqlca.cpy".

      *> SQL status
       01 WS-SQL-STATUS                PIC S9(9) COMP-5.
          88 SQL-STATUS-OK             VALUE    0.
          88 SQL-STATUS-NOT-FOUND      VALUE  100.
          88 SQL-STATUS-DUP            VALUE -803.

      *> SQL declare variables
       
      *EXEC SQL BEGIN DECLARE SECTION END-EXEC.
      *> connect fields with variable length
       01 HV-AREA.
           05 HV-DATE          PIC X(10).
           05 INDICATOR-TAB-DATE.
               20 HV-DATE-I    PIC S9(4) COMP-5.

       01 HV-DBALIAS.
          49 HV-DBALIAS-LEN            PIC S9(4) COMP-5.
          49 HV-DBALIAS-BUF            PIC X(9).
       01 HV-USERID.
          49 HV-USERID-LEN             PIC S9(4) COMP-5.
          49 HV-USERID-BUF             PIC X(20).
       01 HV-PSWD.
          49 HV-PSWD-LEN               PIC S9(4) COMP-5.
          49 HV-PSWD-BUF               PIC X(20).
       
       
      *EXEC SQL END   DECLARE SECTION END-EXEC
                                               

       PROCEDURE DIVISION.

      *>------------------------------------------------------------------------
       MAIN-DB2CONN SECTION.
      *>------------------------------------------------------------------------
           DISPLAY "INSIDE DB2 CONNECTION MODULE"
           
           PERFORM CONNECT THRU CONNECT-EX

           DISPLAY "EXITING DB2 CONNECTION MODULE"
           GOBACK
          .
       MAIN-DB2CONN-EX.
          EXIT.

      *>------------------------------------------------------------------------
       CONNECT SECTION.
      *>------------------------------------------------------------------------

          MOVE 'TESTDB' TO HV-DBALIAS-BUF
          MOVE FUNCTION STORED-CHAR-LENGTH(HV-DBALIAS-BUF)
            TO HV-DBALIAS-LEN

          MOVE 'DB2INST1'   TO HV-USERID-BUF
          MOVE FUNCTION STORED-CHAR-LENGTH(HV-USERID-BUF)
            TO HV-USERID-LEN

          MOVE 'db2admin'     TO HV-PSWD-BUF
          MOVE FUNCTION STORED-CHAR-LENGTH(HV-PSWD-BUF)
            TO HV-PSWD-LEN

          PERFORM SQL-CONNECT
          .
       CONNECT-EX.
          EXIT.

      *>------------------------------------------------------------------------
       SQL-CONNECT SECTION.
      *>------------------------------------------------------------------------
           DISPLAY "INITIATING CONNECTION REQUEST WITH TESTDB"
          
          
      *EXEC SQL 
      *CONNECT TO    :HV-DBALIAS
      *                 USER  :HV-USERID
      *                 USING :HV-PSWD
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 1 TO SQL-STMT-ID 
           MOVE 3 TO SQLDSIZE 
           MOVE 2 TO SQLDA-ID 

           CALL "sqlgaloc" USING
               BY VALUE SQLDA-ID 
                        SQLDSIZE
                        SQL-STMT-ID
                        0

           MOVE 9 TO SQL-HOST-VAR-LENGTH
           MOVE 448 TO SQL-DATA-TYPE
           MOVE 0 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-DBALIAS
            BY VALUE 0
                     0

           MOVE 20 TO SQL-HOST-VAR-LENGTH
           MOVE 448 TO SQL-DATA-TYPE
           MOVE 1 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-USERID
            BY VALUE 0
                     0

           MOVE 20 TO SQL-HOST-VAR-LENGTH
           MOVE 448 TO SQL-DATA-TYPE
           MOVE 2 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-PSWD
            BY VALUE 0
                     0

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 2 TO SQL-INPUT-SQLDA-ID 
           MOVE 5 TO SQL-SECTIONUMBER 
           MOVE 29 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                                                                        

          MOVE SQLCODE TO WS-SQL-STATUS

          DISPLAY "CONNECTION REQUEST RESPONSE CODE SQLCODE : "
           SQLCODE

          DISPLAY "FETCH CURRENT DATE FROM DB2"
          
          
      *EXEC SQL 
      *SELECT CURRENT DATE 
      *         INTO :HV-AREA.HV-DATE 
      *         FROM SYSIBM.SYSDUMMY1
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 2 TO SQL-STMT-ID 
           MOVE 1 TO SQLDSIZE 
           MOVE 3 TO SQLDA-ID 

           CALL "sqlgaloc" USING
               BY VALUE SQLDA-ID 
                        SQLDSIZE
                        SQL-STMT-ID
                        0

           MOVE 10 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 0 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-DATE
            OF
            HV-AREA
            BY VALUE 0
                     0

           MOVE 3 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 1 TO SQL-SECTIONUMBER 
           MOVE 24 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                                                                        

          DISPLAY "GET DATE SQL ENDED WITH SQLCODE :" SQLCODE
          DISPLAY "TODAY'S DATE IS: " HV-DATE OF HV-AREA
          
          DISPLAY "RESETTING DB2 CONNECTION NOW"
          
      *EXEC SQL 
      *CONNECT RESET 
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 3 TO SQL-SECTIONUMBER 
           MOVE 29 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                                                                        
          DISPLAY "CONNECT RESET SQLCODE :" SQLCODE

          .
       SQL-CONNECT-EX.
          EXIT.
       END PROGRAM DB2CONN.
