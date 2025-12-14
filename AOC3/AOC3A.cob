       replace ==:roof:== by ==100==.
       identification division.
       program-id. AOC3A.

       environment division.
       configuration section.
      *special-names. decimal-point is comma.
       repository. function all intrinsic.
       input-output section.
       file-control.
           select INFIL assign to 'input.txt'
           organization is line sequential
           file status is INPUT-FS.

       data division.
       file section.
       FD INFIL.
       01 INDATA.
          05 NUMS pic X(:roof:).
          05 FILLER redefines NUMS.  
             10 N pic 9 occurs :roof: times indexed by N-INDEX.

       working-storage section.
       01 A-ARB.
          05 INPUT-FS         pic XX.
          05 A-COUNT          pic S9(6) comp-4 value ZERO.
          05 A-ACC            pic S9(8) comp-4 value ZERO.
          05 A-START-POS      pic S9(8) comp-4 value ZERO.
          05 A-SAVE-INDEX     pic S9(8) comp-4 value ZERO.
          05 TOGETHER         pic 99.
          05 FILLER redefines TOGETHER.
             10 LARGEST-HIGH     pic 9.
             10 LARGEST-LOW      pic 9.

       01 V-VAXLAR.
          05 FILLER pic X   value ' '.
             88 V-INIT      value ' '.
             88 V-INPUT-EOF value 'E'.

       procedure division.
       A-MAIN section.
           display 'AOC3A'
           open input INFIL
           if INPUT-FS not = '00'
              display INPUT-FS 
              goback
           end-if
           read INFIL at end set V-INPUT-EOF to true end-read
           perform until V-INPUT-EOF
              add 1 to A-COUNT
              move ZERO to LARGEST-HIGH LARGEST-LOW
              compute A-START-POS = :roof: - 1
              perform varying N-INDEX from A-START-POS by -1
                        until N-INDEX < 1
                 if N(N-INDEX) >= LARGEST-HIGH
                    move N(N-INDEX) to LARGEST-HIGH
                    move N-INDEX to A-SAVE-INDEX
                 end-if
              end-perform
              compute A-START-POS = A-SAVE-INDEX + 1
              perform varying N-INDEX from A-START-POS by 1
                        until N-INDEX > :roof:
                 if N(N-INDEX) >= LARGEST-LOW
                    move N(N-INDEX) to LARGEST-LOW
                 end-if
              end-perform
              add TOGETHER to A-ACC
              display TOGETHER 
              read INFIL at end set V-INPUT-EOF to true end-read
           end-perform
           close INFIL
           display A-ACC

           goback
           .
