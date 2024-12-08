       IDENTIFICATION DIVISION.
       PROGRAM-ID. JOUR5.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT InputFile ASSIGN TO
            "/mnt/c/pgmcobol/AoC2024/J5/input.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
          FILE SECTION.
            FD InputFile.
      *       Chaque ligne fait 80 caractères maximum. 
              01 InputRecord PIC X(80).  
       
       WORKING-STORAGE SECTION.
           01 Tableau.
             05 nb-noeud       pic 9(3) value ZERO.
             05 table-noeud OCCURS 100.
                10 noeuds      pic 9(2) value zero.
                10 nb-fils     pic 9(2) value ZERO.
                10 tab-fils    occurs 100 pic 9(2) value zero.

           01 tab-maj-x pic x(200).
           01 tab-maj redefines tab-maj-x occurs 100 pic 9(2).
           01 nb-page-maj pic 9(2) value zero.
           01 RecordCounter PIC 9(04) VALUE 0.
           01  i                        pic 9(6) value ZERO.
           01  j                        pic 9(6) value ZERO.
           01  k                        pic 9(6) value ZERO.
           01  l                        pic 9(6) value ZERO.
           01  w                        pic 9(6) value ZERO.           
           01  COMPTEUR-OCCURENCES      PIC 9(6) VALUE ZERO.
           01 nb-pages                  pic 9(3) value zero.
           01 resultat                  pic 9(6) value zero.
           01 resultat-invalide         pic 9(6) value zero.

           01  noeud-en-cours           pic 9(2).
           01  fils-en-cours            pic 9(2).

           01  WS-EOF PIC A(1).

           01  WS-mode-fichier   PIC 9 VALUE 0.
             88  listing             VALUE 1.
             88  impression          VALUE 0.

           01  WS-trouve   PIC 9 VALUE 0.
             88  trouve             VALUE 1.
             88  pas-trouve         VALUE 0.

           01  WS-imp-maj       PIC 9 VALUE 1.
             88  impression-valide    VALUE 1.
             88  impression-invalide  VALUE 0.          
           
           01 WS-Data PIC X(02).
           01 WS-Data-num redefines WS-Data PIC 9(02).
           

       PROCEDURE DIVISION.
       Main-Logic.
           MOVE 'N' TO WS-EOF
           set listing to true
           OPEN INPUT InputFile
           PERFORM UNTIL WS-EOF = 'Y'
               READ InputFile INTO InputRecord
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM Process-Record
           END-PERFORM
           CLOSE InputFile
           display 'resultat de la mise à jour : ' resultat
           display 'resultat de la mise à jour invalide : '
                  resultat-invalide
           STOP RUN.
       
       Process-Record.
           ADD 1 TO RecordCounter
           display "*" InputRecord "*"
           
           if InputRecord(1:1) is not numeric
              set impression to true
              display "nombre de noeud : " nb-noeud 
              PERFORM VARYING i FROM 1 BY 1
               UNTIL i > nb-noeud
                 display "noeud : " noeuds(i)
                 display "fils  : " WITH NO ADVANCING
                 perform VARYING j from 1 by 1 until j > nb-fils(i)
                    display " " tab-fils(i,j)
                          WITH NO ADVANCING
                 end-perform
                 display " "
              end-perform
           else
              if listing
                 perform traitement-listing
              else
                 if impression
                    perform traitement-impression
                 end-if
              end-if
           end-if
           .

       traitement-listing.          
           initialize noeud-en-cours fils-en-cours
           UNSTRING InputRecord DELIMITED BY "|" INTO
             noeud-en-cours
             fils-en-cours
           END-UNSTRING              
      D     display noeud-en-cours "," fils-en-cours "|" nb-noeud
           if nb-noeud > 0
             set pas-trouve to true
             PERFORM  VARYING i FROM 1 BY 1
                UNTIL i > nb-noeud or trouve
      D          display "recherche " noeuds(i) " " noeud-en-cours 
                if noeuds(i) = noeud-en-cours
                   add 1 to nb-fils(i)
                   move fils-en-cours to tab-fils(i,nb-fils(i))
                   set trouve to true
                end-if
             END-PERFORM
             if pas-trouve
                add 1 to nb-noeud
                move noeud-en-cours to noeuds(nb-noeud)
                add 1 to nb-fils(nb-noeud)
                move fils-en-cours 
                     to tab-fils(nb-noeud,nb-fils(nb-noeud))
             end-if   
           else
             move noeud-en-cours to noeuds(1)
             add 1 to nb-fils(1)
             add 1 to nb-noeud
             move fils-en-cours to tab-fils(1,nb-fils(1))
           end-if
           .

       traitement-impression.
           move 0 to nb-pages
           move all '0' to tab-maj-x
           PERFORM VARYING i FROM 1 BY 1 UNTIL InputRecord(i:1) = " "
              IF InputRecord(i:1) is numeric
                 if InputRecord(i + 1 : 1) is numeric
                    move InputRecord(i:2) to WS-Data
      D              display WS-Data-num
                    add 2 to i
                 else
                    move InputRecord(i:1) to WS-Data(2:1)
      D              display WS-Data-num
                    add 1 to i
                 end-if
                 add 1 to nb-pages
                 move WS-Data-num to tab-maj(nb-pages) 
              END-IF
           END-PERFORM
           set impression-valide to true
           PERFORM VARYING i FROM 1 BY 1
            UNTIL i >= nb-pages or impression-invalide
      D        display " " tab-maj(i) WITH NO ADVANCING
               move tab-maj(i) to noeud-en-cours
      D         display "traitement de : " noeud-en-cours
               compute w = i + 1
               perform varying j from w by 1
                until j > nb-pages or impression-invalide
                  move tab-maj(j) to fils-en-cours
                  set pas-trouve to true
                  perform page-valide
                  if pas-trouve
                     set impression-invalide to true
                  end-if
               end-perform    
           END-PERFORM
           if impression-valide
              display 'impression valide'
              compute i = nb-pages / 2
              add 1 to i
              display "millieu : " i " - " tab-maj(i)
              compute resultat = resultat + tab-maj(i)
           else
              display "Correction en cours"
              perform correction-impression
              compute i = nb-pages / 2
              add 1 to i
              display "millieu invalide : " i " - " tab-maj(i)
              compute resultat-invalide = resultat-invalide + tab-maj(i)        
              display 'impression invalide'
           end-if
           .

       page-valide.
           PERFORM varying k from 1 by 1 until k > nb-noeud or trouve
              if noeuds(k) = noeud-en-cours
      D           display "Recherche de :" fils-en-cours
                 PERFORM VARYING l FROM 1 BY 1
                  UNTIL l > nb-fils(k) or trouve
      D              display " " tab-fils(k,l) 
                    if tab-fils(k,l) = fils-en-cours
                       set trouve to true
                    end-if
                 end-perform
              end-if
           end-perform
           .
       
       correction-impression.
           display '***'
           PERFORM VARYING i FROM 1 BY 1
            UNTIL i >= nb-pages
      D        display " " tab-maj(i) WITH NO ADVANCING
               move tab-maj(i) to noeud-en-cours
      D         display "traitement de : " noeud-en-cours
               compute w = i + 1
               set impression-valide to true
               perform varying j from w by 1
                until j > nb-pages or impression-invalide
                  move tab-maj(j) to fils-en-cours
                  set pas-trouve to true
                  perform page-valide
                  if pas-trouve
      D               display "correction par inversion de "
      D                noeud-en-cours " avec " fils-en-cours
                     move fils-en-cours to tab-maj(i)
                     move noeud-en-cours to tab-maj(j)
                     compute i = i - 1
                     set impression-invalide to true
                  end-if
               end-perform    
           END-PERFORM           
           .
