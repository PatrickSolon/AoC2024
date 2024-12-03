       IDENTIFICATION DIVISION.
       PROGRAM-ID. JOUR3.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHIER-ENTREE ASSIGN
            TO "/mnt/c/pgmcobol/AoC2024/J3/input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FICHIER-ENTREE.
       01  LIGNE-FICHIER.
           05  LIGNE-TEXTE    PIC X(20000).  

       WORKING-STORAGE SECTION.
       01  i                        pic 9(6) value ZERO.
       01  j                        pic 9(6) value ZERO.
       01  k                        pic 9(6) value ZERO.
       01  COMPTEUR-OCCURENCES      PIC 9(6) VALUE ZERO.
       01  WS-POSITION              PIC 9(6) VALUE ZERO.
       01  LONGUEUR-CARACTERE       PIC 9(6) VALUE ZERO.
       01  CHAINE-RECHERCHE         PIC X(3) VALUE "mul".
       01  ws-first-num             PIC 9(8) value ZERO.
       01  ws-secnd-num             PIC 9(8) value ZERO.
       01  WS-TOTAL                 PIC 9(10) value ZERO.
       01  WS-TOTAL-CORR            PIC 9(10) value ZERO.
       01  ws-num-x                 PIC x(1).
       01  ws-num redefines ws-num-x PIC 9(1).
       
       01  WS-MUL   PIC 9 VALUE 0.
           88  MUL-VALIDE       VALUE 1.
           88  MUL-INVALIDE     VALUE 0.
       01  WS-DO   PIC 9 VALUE 0.
           88  DO-ACTIF       VALUE 1.
           88  DO-INACTIF     VALUE 0.
       01  WS-FICHIER   PIC 9 VALUE 0.
           88  FIN-FICHIER       VALUE 1.
           88  NOT-FIN-FICHIER   VALUE 0.

       PROCEDURE DIVISION.
       DEBUT.
           OPEN INPUT FICHIER-ENTREE
           PERFORM LECTURE-ET-TRAITEMENT
           DISPLAY "WS-TOTAL = " WS-TOTAL
           DISPLAY "WS-TOTAL CORRIGé = " WS-TOTAL-CORR
           CLOSE FICHIER-ENTREE
           STOP RUN.

       LECTURE-ET-TRAITEMENT.
           SET NOT-FIN-FICHIER TO TRUE
           SET DO-ACTIF to TRUE
           PERFORM UNTIL FIN-FICHIER
                 READ FICHIER-ENTREE INTO LIGNE-TEXTE
                     AT END
                         SET FIN-FICHIER TO TRUE
                     NOT AT END
                         PERFORM RECHERCHE-DANS-LIGNE
      D                  DISPLAY "Nombre d'occurences dans la ligne : "
                            COMPTEUR-OCCURENCES
                         MOVE ZERO TO COMPTEUR-OCCURENCES
                 END-READ
           END-PERFORM.

       RECHERCHE-DANS-LIGNE.
           MOVE ZERO TO WS-POSITION
           MOVE 20000 TO LONGUEUR-CARACTERE
      D     DISPLAY 'Longeur de la ligne : ' LONGUEUR-CARACTERE

           PERFORM VARYING WS-POSITION FROM 1 BY 1
               UNTIL WS-POSITION > LONGUEUR-CARACTERE
               if LIGNE-TEXTE(WS-POSITION:4)  = 'do()'
                  set DO-ACTIF to TRUE
      D            display 'DO Actif'
                  add 4 to WS-POSITION
               end-if
               if LIGNE-TEXTE(WS-POSITION:7)  = "don't()"
                  set DO-INACTIF to TRUE
      D            display 'DO Inactif'
                  add 7 to WS-POSITION
               end-if
               IF LIGNE-TEXTE(WS-POSITION:3)  = CHAINE-RECHERCHE
                  SET MUL-VALIDE to TRUE
                  display LIGNE-TEXTE(WS-POSITION:12)
                  ADD 3 to WS-POSITION
      D            display LIGNE-TEXTE(WS-POSITION:3)
                  perform recherche-multiplication
                  if MUL-VALIDE
                     COMPUTE WS-TOTAL = WS-TOTAL +
                             (ws-first-num * ws-secnd-num)
                     if DO-ACTIF
                        COMPUTE WS-TOTAL-CORR = WS-TOTAL-CORR +
                             (ws-first-num * ws-secnd-num)
                     end-if 
      D               display '1er num : ' ws-first-num
      D                       ' 2e num : ' ws-secnd-num
                     ADD 1 TO COMPTEUR-OCCURENCES
                  end-if
               else
                  SET MUL-INVALIDE to TRUE
               END-IF
           END-PERFORM.

       recherche-multiplication.
           if LIGNE-TEXTE(WS-POSITION:1) = "("
              ADD 1 TO WS-POSITION
      D        display LIGNE-TEXTE(WS-POSITION:3)
              if LIGNE-TEXTE(WS-POSITION + 1:1) = ','
                 OR LIGNE-TEXTE(WS-POSITION + 2:1) = ','
                 OR LIGNE-TEXTE(WS-POSITION + 3:1) = ','
                 move 0 to ws-first-num
                 perform recherche-first-num
                 ADD 1 TO WS-POSITION
      D           display LIGNE-TEXTE(WS-POSITION:3)
                 if LIGNE-TEXTE(WS-POSITION + 1:1) = ')'
                    OR LIGNE-TEXTE(WS-POSITION + 2:1) = ')'
                    OR LIGNE-TEXTE(WS-POSITION + 3:1) = ')'
                    move 0 TO ws-secnd-num
                    perform recherche-secnd-num
                 else
                    set MUL-INVALIDE to TRUE
                 end-if
              else
                set MUL-INVALIDE to TRUE   
              end-if
           else
              set MUL-INVALIDE to TRUE
           end-if.



       recherche-first-num.
           perform varying i from 0 by 1 
             until LIGNE-TEXTE(WS-POSITION + i:1) = ','
             if LIGNE-TEXTE(WS-POSITION + i:1) is numeric
                evaluate TRUE
                  when i = 0
                     move LIGNE-TEXTE(WS-POSITION + i:1) to ws-num-x
                     add ws-num to ws-first-num
                  when i = 1
                     move LIGNE-TEXTE(WS-POSITION + i:1) to ws-num-x
                     compute ws-first-num = ws-first-num * 10
                     add ws-num to ws-first-num
                  when i = 2
                     move LIGNE-TEXTE(WS-POSITION + i:1) to ws-num-x
                     compute ws-first-num = ws-first-num * 10
                     add ws-num to ws-first-num                     
                end-evaluate
             end-if
           end-perform
           ADD i to WS-POSITION         
           .

       recherche-secnd-num.
           perform varying i from 0 by 1 
             until LIGNE-TEXTE(WS-POSITION + i:1) = ')'
             if LIGNE-TEXTE(WS-POSITION + i:1) is numeric
                evaluate TRUE
                  when i = 0
                     move LIGNE-TEXTE(WS-POSITION + i:1) to ws-num-x
                     add ws-num to ws-secnd-num
                  when i = 1
                     move LIGNE-TEXTE(WS-POSITION + i:1) to ws-num-x
                     compute ws-secnd-num = ws-secnd-num * 10
                     add ws-num to ws-secnd-num
                  when i = 2
                     move LIGNE-TEXTE(WS-POSITION + i:1) to ws-num-x
                     compute ws-secnd-num = ws-secnd-num * 10
                     add ws-num to ws-secnd-num                     
                end-evaluate
             end-if
           end-perform
           ADD i to WS-POSITION   
           .
        