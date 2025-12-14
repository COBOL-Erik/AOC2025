       identification division.
       program-id. AOC1B.

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
       FD  INFIL.
       01  INDATA.
           05 LETTER pic X.
              88 L-LEFT  value 'L'.
              88 L-RIGHT value 'R'.
           05 CLICKS PIC XXX.

       working-storage section.
       01 A-ARB.
          05 INPUT-FS         pic XX.
          05 A-COUNT          pic S9(6) comp-4 value ZERO.
          05 A-ZERO-COUNTER   pic S9(6) comp-4 value ZERO.
          05 A-ANS            pic S9(4) comp-4 value ZERO.
          05 ARROW            pic 99           value 50.
          05 CLICKS9          pic 9(3)         value ZERO.

       01 V-VAXLAR.
          05 FILLER pic X   value ' '.
             88 V-INIT      value ' '.
             88 V-INPUT-EOF value 'E'.

       procedure division.
       A-MAIN section.
           display 'AOC1B'
           move 50 to ARROW 
           open input INFIL
           if INPUT-FS not = '00'
              display INPUT-FS 
              goback
           end-if
           read INFIL at end set V-INPUT-EOF to true end-read
           perform until V-INPUT-EOF
              add 1 to A-COUNT
              move CLICKS to CLICKS9
              perform until CLICKS9 < 100
                 add 1 to A-ZERO-COUNTER 
                 subtract 100 from CLICKS9
              end-perform
              if L-RIGHT
                 compute A-ANS = ARROW + CLICKS9
                 if A-ANS > 100 AND ARROW NOT = 0
                    add 1 to A-ZERO-COUNTER 
                 end-if
              else
                 compute A-ANS = ARROW - CLICKS9
                 if A-ANS < 0 AND ARROW NOT = 0
                    add 1 to A-ZERO-COUNTER
                 end-if
              end-if
              compute ARROW = mod(A-ANS, 100)
              if ARROW = 0
                 add 1 to A-ZERO-COUNTER
              end-if
              read INFIL at end set V-INPUT-EOF to true end-read
           end-perform
           close INFIL
           display A-ZERO-COUNTER 
           goback
           .
