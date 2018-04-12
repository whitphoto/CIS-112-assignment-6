       identification division.
       program-id. Program1.

       Input-output Section.
       file-control.
           Select tablefile assign to "tablefile.txt"
                  Organization is line sequential.
           select transactionfile assign to "transactionfile.txt"
                  Organization is line sequential.
           select outfile assign to "outfile.txt"
                  Organization is line sequential.
          
       configuration section.

       data division.
       File Section.
       
       FD tablefile.
       01 reg-rec-in.
           05 reg-class-in     pic xx.
           05 reg-rate-in      pic 9v99.     
       
       FD transactionfile.
       01 trans-rec-in.
           05 trans-class-in   pic 99.
           05 trans-name-in    pic x(20).
           05 trans-weight-in  pic 9(5).
           05 trans-desc-in    pic x(13).
           
           
       FD outfile.
       01 output-rec           pic x(49).
       
       working-storage section.
       
       01 EOF              pic x value "n".
       
       01 blank-line       pic x(49).
       
       01 header-1.
           05 filler       pic x(15) value space.
           05 filler       pic x(19) value 'REGISTRATION REPORT'.
           05 filler       pic x(15) value space.
           
       01 header-2.
           05 filler       pic x(7) value 'VEHICLE'.
           05 filler       pic x(42) value spaces.
           
       01 header-3.
           05 filler       pic x(1) value space.
           05 filler       pic x(5) value 'CLASS'.
           05 filler       pic x(12) value spaces.
           05 filler       pic x(4) value 'NAME'.
           05 filler       pic x(10) value spaces.
           05 filler       pic x(6) value 'WEIGHT'.
           05 filler       pic x(6) value spaces.
           05 filler       pic x(3) value 'FEE'.
           05 filler       pic x(3) value spaces.
           
       01 detail-line.
           05 filler       pic x(3) value spaces.
           05 class-out    pic x(2).
           05 filler       pic x(5) value spaces.
           05 name-out     pic x(20).
           05 filler       pic x(2) value spaces.
           05 weight-out   pic zz,zzz.
           05 filler       pic xx value spaces.
           05 fee-out      pic $zz,zzz.99.
       
       01 ws-reg-table.
           05 reg-table occurs 15 times indexed by t-idx.
                10 rt-veh-class pic xx.
                10 rt-reg-rate  pic 9v99.
       
       01 ws-reg-rate      pic 9v99.
       
       01 more-recs        pic xxx value "yes".
                
       procedure division.

       
       100-main.
           open input tablefile, transactionfile.
           open output outfile.
           
           perform 400-header.
           
           perform 200-init-table.
       
           perform until eof = "y"
           read transactionfile
           at end move "y" to EOF
           Not at end perform 300-vehic-reg
                  write output-rec from detail-line
           end-read
           end-perform.
           close tablefile, transactionfile, outfile. 
           stop run.
           
       200-init-table.
           perform varying t-idx from 1 by 1 until t-idx > 15 or more-recs = "no"
           read tablefile
           at end move "no" to more-recs
           not at end 
           move reg-class-in to rt-veh-class (t-idx)
           move reg-rate-in to rt-reg-rate (t-idx).
         
           
       300-vehic-reg.
           set t-idx to 1.
           search reg-table
           when rt-veh-class (t-idx) = trans-class-in 
               move trans-class-in to class-out
               move trans-name-in to name-out
               move trans-weight-in to weight-out
               Compute fee-out = rt-reg-rate (t-idx) * trans-weight-in. 
           
        
               
       
       
       400-header.
           write output-rec from header-1
           write output-rec from blank-line
           write output-rec from header-2
           write output-rec from header-3
           write output-rec from blank-line.
           

        

       end program Program1.