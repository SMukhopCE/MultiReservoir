subroutine constr_res(nparam,index_cons,decision_var,gcons)
USE My_variables
implicit doubleprecision(a-h,o-z)

Real*8 decision_var(nparam),total_deficit(nensem),gcons

! check whether the current value of iteration is same as that of previous iteration
! If so , just return the current value of the constraint using the variable index_cons.

!gcons = -0.1

!return

ivar_status = icheck_var_status(nparam,decision_var,temp_decision_var)

if(ivar_status.eq.0)then

	gcons = cons_global(index_cons)
!	gcons = -0.1


    return

end if

! if the decision_variable value has changed then perform simulation

nsimul_block = nres +nfnode

!call array_int_ini(ntemp,isimul_status,0)

itemp_type = 0
itemp_id = 0



iflow_set = 0
icurrent_id = 0
icurrent_type = 0


 call array_ini(nensem,total_deficit,0.0d0)
 call array_ini(nensem,value_net,0.0d0)
 call array_ini(ncons,cons_global,0.0d0)

! Initialize controlled and uncontrolled flows for each watershed

Do i = 1,nwatershed

		call array_ini(ntime,my_flow_set(i)%controlled_flows,0.0d0)

		Do j = 1,nensem

			call array_ini(ntime,my_flow_set(i)%uncontrolled_flows(1,j),0.0d0)

		end do

		parallel_track(i)%order_type = 0

		parallel_track(i)%order_id = 0

end do


icount = 0

! Loop for simulation at each junction node and reservoir

Do i = 1, nsimul_block


	iprev_id = icurrent_id
	iprev_type = icurrent_type

	icurrent_type = my_network_order(i)%order_type
	icurrent_id = my_network_order(i)%order_id

	if(icurrent_type.eq.5)then
	
	        call node_simul_module(icurrent_type,icurrent_id,iprev_id, &
	   iprev_type, nparam,decision_var)

	end if
	
	if(icurrent_type.eq.3)call reservoir_simul_module(icurrent_type,icurrent_id, iprev_id, &
	   iprev_type,nparam,decision_var,total_deficit,nend_cons)

    if(icurrent_type.eq.3)icount = icount +1
	ensem = nensem
    ! Calculation for end of time steps storage constraints
    if(icurrent_type.eq.3)cons_global(icount) = (nend_cons/ensem) - my_reservoir(icurrent_id)%storage_prob
		
end do

call deficit_splitter(total_deficit,decision_var,nparam)

!call expected_benefits(nparam,1,decision_var,fj)


	gcons = cons_global(index_cons)
!	gcons = -0.1


return
end

! Subroutine for reservoir simulation

subroutine reservoir_simul_module(icurrent_type,icurrent_id,iprev_id, &
	   iprev_type, nparam,decision_var,total_deficit,nend_cons)
USE My_variables
implicit doubleprecision(a-h,o-z)

double precision decision_var(nparam),q(ntime),simul_deficit(ntime),simul_evapo(ntime)
double precision simul_stor(ntime), simul_spill(ntime),rate_area(ntime),release(ntime)
double precision total_deficit(nensem)

	nparent = my_reservoir(icurrent_id)%nparent



ijump = 0
iadd = 0

	DO i1 = 1,nparent
			
		if((iprev_type.eq.my_reservoir(icurrent_id)%parent_type(i1)).and. &
			(iprev_id.eq.my_reservoir(icurrent_id)%parent_id(i1)))ijump = 1

	end do

	if(ijump.eq.0)then
	
		if((iprev_type.ne.0).and.(iprev_id.ne.0))then

			parallel_track(iflow_set)%order_id = iprev_id
			parallel_track(iflow_set)%order_type = iprev_type

		end if

			iflow_set = iflow_set +1
        
	end if 

Do i = 1,nensem

	Do j = 1,ntime

		my_flow_set(iflow_set)%uncontrolled_flows(j,i) = 0
        end do
end do

inodeparent = 0

	DO i1 = 1,nparent

		iparent_type = my_reservoir(icurrent_id)%parent_type(i1)
                if (iparent_type == 5)  then 
                        inodeparent =1
                else 
                        inodeparent = 0
                end if 
                

        end do
if (inodeparent .ne.1) then
	Do j = 1,ntime

		my_flow_set(iflow_set)%controlled_flows(j) = 0
        end do
end if


        ! Loop for adding controlled and uncontrolled flow from parents
	DO i1 = 1,nparent

		iparent_type = my_reservoir(icurrent_id)%parent_type(i1)
		iparent_id   = my_reservoir(icurrent_id)%parent_id(i1)

! Prepare the inflow sets

		iadd = 0

		if(iparent_type.eq.1)call add_uncontrolled_flows  &
		  (iparent_type,iparent_id,decision_var,nparam)


		if(iparent_type.eq.3) then

!		    Do j1 = 1,nwatershed

!				if((parallel_track(j1)%order_type.eq.iparent_type).and. &
!				  (parallel_track(j1)%order_id.eq.iparent_id))call add_all_flows &
!				  (j1,iparent_type,iparent_id,decision_var,nparam)

!			end do
			call add_controlled_flows(iparent_type,iparent_id, decision_var,nparam)

		end if

		if(iparent_type.eq.5) then

		    Do j1 = 1,nwatershed

				if((parallel_track(j1)%order_type.eq.iparent_type).and. &
				  (parallel_track(j1)%order_id.eq.iparent_id))call add_all_flows &
				  (j1,iparent_type,iparent_id,decision_var,nparam)

			end do

            if((iparent_type.ne.iprev_type).and.(iparent_id.ne.iprev_id))call  &
			add_controlled_flows(iparent_type,iparent_id, decision_var,nparam)

		end if


		if((iparent_type.eq.4).or.(iparent_type.eq.13))call add_controlled_flows  &
		  (iparent_type,iparent_id, decision_var,nparam)

	end do

! Prepare the outflow sets

call array_ini(ntime,release, 0.0d0)

	nchild = my_reservoir(icurrent_id)%nchild
        
! Loop for adding the releases to child nodes
	Do i1 = 1,nchild

		ichild_type = my_reservoir(icurrent_id)%child_type(i1)
		ichild_id   = my_reservoir(icurrent_id)%child_id(i1)

call calculate_outflows(ichild_type,ichild_id,icurrent_id, icurrent_type, release,decision_var,nparam)

	end do


    storage_max = my_reservoir(icurrent_id)%storage_max
	storage_ini = my_reservoir(icurrent_id)%current_storage

Do j = 1,ntime

		rate_area(j) = my_reservoir(icurrent_id)%evaporation_rate(j)
end do

! Variable for couting the number of time not meeting target storage constraints
nend_cons = 0

! Loop for reservoir simulation
Do i = 1,nensem

	Do j = 1,ntime

		q(j) = my_flow_set(iflow_set)%uncontrolled_flows(j,i) + &
		       my_flow_set (iflow_set)%controlled_flows(j)

	end do

	call reser_simul(icurrent_id, q, ntime,release,storage_max, storage_ini,rate_area, &
					 simul_stor,simul_spill, simul_evapo, simul_deficit,iflag)
        
        print *, my_reservoir(icurrent_id)%name
        print *, 'storage'
        Do k = 1, 3
                print *, simul_stor(k)
        END DO
        print *, 'inflow'
        Do k = 1, 3
                print *, q(k)
        END DO
        print *, 'release'
        Do k = 1, 3
                print *, release(k)
        END DO
        print *, 'spill'
        Do k = 1, 3
                print *, simul_spill(k)
        END DO



	DO i1 = 1,nchild

		ichild_type = my_reservoir(icurrent_id)%child_type(i1)
                ichild_id   = my_reservoir(icurrent_id)%child_id(i1)

                if (ichild_type == 4)  then 

                call hydropower(ichild_type,ichild_id,icurrent_id, icurrent_type, simul_stor, release, nensem)

                        exit
                end if 
        end do



	if(iflag.eq.1)then

		Do k=1,ntime

			total_deficit(i) = total_deficit(i)+simul_deficit(k)

		end do

	end if

    if(simul_stor(ntime).lt.my_reservoir(icurrent_id)%target_storage)nend_cons = nend_cons + 1

end do



return
end


! Subroutine for hydropwer calculation
subroutine hydropower(iblock_type,iblock_id,icurrent_id, icurrent_type,simul_stor,release,nensemble)
Use My_variables
implicit doubleprecision(a-h,o-z)
double precision simul_hydropower(ntime), simul_stor(ntime), release(ntime)
double precision head_elevation(ntime)

!Calculating head elevation

        Do j = 1, 3
         
                    head_elevation(j)=my_reservoir(icurrent_id)%elevation_storage_coeff(1)*(simul_stor(j)**2)+&
                        my_reservoir(icurrent_id)%elevation_storage_coeff(2)*simul_stor(j)+ &
                        my_reservoir(icurrent_id)%elevation_storage_coeff(3)
        
        End do


! Loop for hydropower calculation

        simul_hydropower(1) =  my_user(iblock_id)%generator_efficiency*62.5*1.356*1000/2/31/1000000*release(1)*& 
           ( head_elevation(1) - my_user(iblock_id)%tail_elevation(1,nensemble))

        simul_hydropower(2) =  my_user(iblock_id)%generator_efficiency*62.5*1.356*1000/2/30/1000000*release(2)*& 
           (head_elevation(2) - my_user(iblock_id)%tail_elevation(2,nensemble))

        simul_hydropower(3) =  my_user(iblock_id)%generator_efficiency*62.5*1.356*1000/2/31/1000000*release(3)*& 
           (head_elevation(3) - my_user(iblock_id)%tail_elevation(3,nensemble))





        Do j = 1, 3
                if  (simul_hydropower(j) .LT. 0.0)then 
                        simul_hydropower(j) = 0.0
                end if 
        end do
        
        
        print *, my_user(icurrent_id)%name
        print *, 'power(MW)'
        Do k = 1, 3
                print *, simul_hydropower(k)
        END DO

return

end

! Subroutine for simulation at each junction node

subroutine node_simul_module(icurrent_type,icurrent_id,iprev_id, &
	   iprev_type, nparam,decision_var)
USE My_variables
implicit doubleprecision(a-h,o-z)

double precision decision_var(nparam),release(ntime),inflow(ntime)


	nparent = my_node(icurrent_id)%nparent


ijump = 0
iadd = 0

	DO i1 = 1,nparent
			
		if((iprev_type.eq.my_node(icurrent_id)%parent_type(i1)).and. &
			(iprev_id.eq.my_node(icurrent_id)%parent_id(i1)))ijump = 1

	end do

	if(ijump.eq.0)then
	
		if((iprev_type.ne.0).and.(iprev_id.ne.0))then

			parallel_track(iflow_set)%order_id = iprev_id
			parallel_track(iflow_set)%order_type = iprev_type

		end if

			iflow_set = iflow_set +1
        

	end if 
if (iflow_set .NE. 1) then
        Do j = 1,ntime

        	my_flow_set (iflow_set-1)%controlled_flows(j) = 0.0

        end do
end if 

Do j = 1,ntime

	my_flow_set (iflow_set)%controlled_flows(j) = 0.0

end do






! Add controlled and uncontrolled flows from parents
	DO i1 = 1,nparent

		iparent_type = my_node(icurrent_id)%parent_type(i1)
		iparent_id   = my_node(icurrent_id)%parent_id(i1)

! Prepare the inflow sets

		iadd = 0

 		if(iparent_type.eq.1)call add_uncontrolled_flows  &
		  (iparent_type,iparent_id,decision_var,nparam)

!	if(iparent_type.eq.3) then

!	    Do j1 = 1,nwatershed

!			if((parallel_track(j1)%order_type.eq.iparent_type).and. &
!			  (parallel_track(j1)%order_id.eq.iparent_id))call add_all_flows &
!			  (j1,iparent_type,iparent_id,decision_var,nparam)

!		end do

!            call  add_controlled_flows(iparent_type,iparent_id, decision_var,nparam)

!		end if

		if(iparent_type.eq.5) then

		    Do j1 = 1,nwatershed

				if((parallel_track(j1)%order_type.eq.iparent_type).and. &
				  (parallel_track(j1)%order_id.eq.iparent_id))call add_all_flows &
				  (j1,iparent_type,iparent_id,decision_var,nparam)

			end do

            if((iparent_type.ne.iprev_type).and.(iparent_id.ne.iprev_id))call  &
			add_controlled_flows(iparent_type,iparent_id, decision_var,nparam)

		end if



		if((iparent_type.eq.4).or.(iparent_type.eq.13))call add_controlled_flows  &
		  (iparent_type,iparent_id, decision_var,nparam)

	end do

! Prepare the outflow sets from uses

call array_ini(ntime,release, 0.0d0)
nchild = my_node(icurrent_id)%nchild

! Loop for adding releases to child nodes
	Do i1 = 1,nchild

		ichild_type = my_node(icurrent_id)%child_type(i1)
		ichild_id   = my_node(icurrent_id)%child_id(i1)


        if(ichild_type.eq.4)then

			Do j  = 1,ntime

!                fract = my_user(ichild_id)%demand_fract(j)
!		       if(ichild_type.eq.4)release (j) = release(j) + fract*decision_var(ichild_id)
                        if(ichild_type.eq.4)release (j) = release(j) + decision_var((ichild_id*3)+j)

			end do

		end if

!		if((ichild_type.eq.5).and.(ichild_id.eq.3))then
!			
!				Do j  = 1,ntime

!					release(j) = release(j) + (0.0/12)

!				end do
!		end if

	end do

! Loop for flow mass balance
Do j= 1,ntime

    temp = my_flow_set(iflow_set)%controlled_flows(j)

	temp = temp - release(j)

	if(temp.le.0)then 
		
		my_flow_set(iflow_set)%controlled_flows(j)= 0.0

	else
		my_flow_set(iflow_set)%controlled_flows(j)= temp

	end if


end do

return

end

subroutine add_uncontrolled_flows(iblock_type,iblock_id, decision_var,nparam)
Use My_variables
implicit doubleprecision(a-h,o-z)

! Loop for adding natural flow 
DO i = 1,nensem

	DO j = 1,ntime
		temp = my_flow_set(iflow_set)%uncontrolled_flows(j,i) 
		temp = temp + my_watershed(iblock_id)%natural_inflows(j,i)
		my_flow_set(iflow_set)%uncontrolled_flows(j,i) = temp

	end do

end do

return

end

subroutine add_controlled_flows(iblock_type,iblock_id,decision_var,nparam)
Use My_variables
implicit doubleprecision(a-h,o-z)

double precision decision_var(nparam)

    
! Loop for adding controlled flows
DO j = 1,ntime

  if(iblock_type.eq.4)then

  nlags = my_user(iblock_id)%nlags

    if(j.gt.nlags)then
		   
	   temp1 = 0.0d0
           if (nlags == 0) then
!                   temp1 = my_user(iblock_id)%demand_fract(j) &
!					*decision_var(iblock_id)
                    temp1 = decision_var((iblock_id-1)*3+j)    
           else 
	        Do k = 1,my_user(iblock_id)%nlags
				
	        fract = (1-my_user(iblock_id)%ffraction(k))
		rflow = my_user(iblock_id)%demand_fract(j-k) &
				*decision_var(iblock_id)*fract

		temp1 = temp1 + rflow

	        end do

	   end if 
	temp = temp1

	else
		  
	   temp = 0.0
		  
    end if 

  end if 

   if(iblock_type.eq.13)temp = my_interbasin(iblock_id)%average_flow(j)
   if(iblock_type.eq.3)temp = decision_var(iblock_id+nuser*3)/12

!	if((iblock_type.eq.5).and.(iblock_id.eq.3))then
!	      temp = 0.0
!		  GO to 15
!	endif

   if(iblock_type.eq.5)temp = my_flow_set(iflow_set)%controlled_flows(j)

15	my_flow_set(iflow_set)%controlled_flows(j) = temp +my_flow_set(iflow_set)%controlled_flows(j)

end do

return

end

subroutine calculate_outflows(iblock_type,iblock_id,icurrent_id, icurrent_type,release,decision_var,nparam)
Use My_variables
implicit doubleprecision(a-h,o-z)

double precision decision_var(nparam),release(ntime)

! Loop for release calculation
	DO j = 1,ntime

!       if(iblock_type.eq.4)temp = my_user(iblock_id)%demand_fract(j) &
!					*decision_var(iblock_id)
           if(iblock_type.eq.4)temp =decision_var((iblock_id-1)*3 + j)

	   if(iblock_type.eq.3)temp = decision_var(icurrent_id+nuser*3)/12
	   if(iblock_type.eq.5)temp = decision_var(icurrent_id+nuser*3)/12
	   if(iblock_type.eq.12)temp = decision_var(icurrent_id+nuser)/12

        temp1 = release(j)
		temp = temp + temp1
		release(j) = temp

	end do

return

end

! Add controlled and uncontrolled flows from another flowset
subroutine add_all_flows(iadd_set,iparent_type,iparent_id,decision_var,nparam)
Use My_variables
implicit doubleprecision(a-h,o-z)

double precision decision_var(nparam)

! Add uncontrolled_flows

Do j = 1,ntime

!	Do k = 1,nensem


!		temp = my_flow_set(iadd_set)%uncontrolled_flows(j,k)
!		temp1 = my_flow_set(iflow_set)%uncontrolled_flows(j,k)
!		my_flow_set(iflow_set)%uncontrolled_flows(j,k) = temp + temp1

!	end do

		temp = my_flow_set(iadd_set)%controlled_flows(j)
		temp1 = my_flow_set(iflow_set)%controlled_flows(j)
		my_flow_set(iflow_set)%controlled_flows(j) = temp + temp1

end do

return

end


! Check to ensure decision_var and temp_decision_var are the same

Integer function icheck_var_status(nparam, decision_var,temp_decision_var)
implicit doubleprecision (a-h,o-z)

double precision decision_var(nparam),temp_decision_var(nparam)

icheck_var_status = 0

Do i = 1, nparam

	if(decision_var(i).ne.temp_decision_var(i))goto 12

End do

return

12   icheck_var_status = 1

 Do i = 1,nparam

	temp_decision_var(i) = decision_var(i)

End do

return

end 
! Subroutine for reservoir simulation
subroutine reser_simul(icurrent_id,q, ntime,release,storage_max, storage_ini,rate_area, &
					 simul_stor,simul_spill, simul_evapo, simul_deficit,iflag)
implicit doubleprecision(a-h,o-z)

double precision q(ntime), rate_area(ntime),release(ntime) 
double precision simul_stor(ntime),simul_evapo(ntime)
double precision simul_deficit(ntime), simul_spill(ntime)

common/et_est/et_rate,storage_pre,current_flow,current_release,bal_net, iwrite


iflag = 0

storage_pre = storage_ini

! Loop for reservoir mass balance
Do j = 1, ntime
                  
	sum_release = release(j)
                
	bal_net = q(j) - sum_release 
	current_flow  = q(j)
	current_release = sum_release

    et_rate  = rate_area(j)
 
    call evaporation_iter(icurrent_id, storage_current, storage_max,evapo_current, spill, deficit) 			 

	simul_stor(j) = storage_current
	simul_evapo(j) = evapo_current
	simul_spill(j) = spill
	simul_deficit(j) = deficit

    if(deficit.gt.0.0)iflag = 1

		storage_pre = storage_current

	end do


return

end


Subroutine deficit_splitter(total_deficit,decision_var,nparam)
Use My_variables
implicit doubleprecision(a-h,o-z)

doubleprecision total_deficit(nensem),simul_def_user(nuser)
doubleprecision deficit_split_user(nres_level,nuser),decision_var(nparam)

Integer ilevel_fail(nres_level),idef_user(nuser)

! Loop for distributing deficit among users
Do k1 = 1,nensem

call array_ini(nuser,simul_def_user,0.0d0)
call array_ini_two(nres_level,nuser,deficit_split_user,0.0d0)
call array_int_ini(nres_level,ilevel_fail,0)
call array_int_ini(nuser,idef_user,0)

if(total_deficit(k1).ne.0)then

		account_def = 0.0


 Do i = 1,nres_level

	  current_def = 0.0
      ilevel_fail(i) = ilevel_fail(i)+1.0
      
	   Do j = 1,nuser

        temp = my_user(j)%restr_fract(i)*decision_var(j)
		 simul_def_user(j) = simul_def_user(j)+temp
		 current_def = current_def+ temp
		
		  deficit_split_user(i,j) = temp

        end do
          
         account_def = account_def+current_def

         

	if(account_def.ge.total_deficit(k1))then
         ilevel = i
        adjust_def = account_def - total_deficit(k1)
		go to 17
	end if

		

 end do 

Go to 25
!if(account_def.lt.total_deficit(k1))then

!	Write(*,*)'Increase the Restriction Level or the Restriction fraction'
!	Stop

!end if 

        !distribute deficits to users
17    Do j = 1,nuser

			temp1 = adjust_def/current_def
			temp = my_user(j)%restr_fract(ilevel)*decision_var(j)*temp1
			simul_def_user(j) = simul_def_user(j) - temp
			deficit_split_user(ilevel,j) = my_user(j)%restr_fract(ilevel)*decision_var(j) - temp
	 end do

25 continue


	Do j = 1,nuser

			if(simul_def_user(j).ge.my_user(j)%con_res_vol)idef_user(j) = idef_user(j)+ 1.0

	end do

end if


call functn(decision_var,nparam,idef_user,ilevel_fail,deficit_split_user,ben_net,simul_def_user)

value_net(k1) = ben_net

end do


k2 = nres

ensem = nensem

! Loop for calculating failure probability constraints
Do i = 1,nuser

    k2 = k2 +1
    cons_global(k2) = (idef_user(i)/ensem) - my_user(i)%failure_prob

end do

! Loop for calculating target restriction constraints
Do i = 1,nres_level

	k2 = k2 +1
    cons_global(k2) = (ilevel_fail(i)/ensem)-my_reservoir(1)%tar_restr_prob(i)

end do


return
end

! subroutine for calculating net benefits
subroutine functn(decision_var,nparam,idef_user,ilevel_fail,deficit_split_user,ben_net,simul_def_user)
Use my_variables
implicit doubleprecision(a-h,o-z)

doubleprecision decision_var(nparam),deficit_split_user(nres_level,nuser)
doubleprecision simul_def_user (nuser)

Integer idef_user(nuser),ilevel_fail(nres_level)

ben_net = 0.0

DO i = 1,nuser

  if(idef_user(i).ne.1)then
		ben_net = ben_net + my_user(i)%tariff*decision_var(i)			
  else
        temp1 = simul_def_user(i) - my_user(i)%con_res_vol
		temp = my_user(i)%penalty + temp1*my_user(i)%penalty_compen
		ben_net = ben_net - temp
  endif

  Do k = 1,nres_level

	if(ilevel_fail(k).eq.1)then

	   temp = my_user(i)%res_compensation(k)*deficit_split_user(k,i)
       ben_net = ben_net - temp

	endif

  end do
	 

end do

return

end

subroutine expected_benefits(nparam,j,decision_var,fj)
Use my_variables
implicit doubleprecision(a-h,o-z)

doubleprecision decision_var(nparam),fj,x1a,x1b,x1c,x1cs


fj=0.0

!ben_net = 0.0

!DO i = 1,nuser

!		ben_net = ben_net + my_user(i)%tariff*decision_var(i)			

!end do

call constr_res(nparam,1,decision_var,gcons)


!call stat_(nensem,value_net,x1a,x1s,x1cv,x1cs)

!x1a = ben_net
total_value = 0

DO i =1, nensem
        
        total_value =  total_value+value_net(i)
       
END DO

!fj = -x1a
fj = -total_value

return

end


Subroutine evaporation_iter(icurrent_id,storage_current,storage_max,evapo_current, spill,deficit) 			 
! Use NUmerical_libraries
USE My_variables



implicit doubleprecision(a-h,o-z)
external et_function,lsjac

common/et_est/et_rate,storage_pre,current_flow,current_release,bal_net, iwrite

alpha = my_reservoir(icurrent_id)%storage_area_coeff(1)
beta = my_reservoir(icurrent_id)%storage_area_coeff(2)
!alpha = 0.001
!beta = 0.002

 
 if(storage_pre.lt.0.001)storage_pre=0.0

		temp = current_flow - current_release + storage_pre

		storage_min = 0.01
    
		temp_min = (storage_pre + storage_min)/2.0

		rmin = storage_min + et_cal(et_rate,temp_min, alpha, beta)

		temp_max = (storage_pre + storage_max)/2.0

		rmax = storage_max+ et_cal(et_rate,temp_max, alpha, beta)

if(rmin.gt.temp)then
		x=0.0
		go to 12
end if

if(rmax.lt.temp)then
	  x= storage_max
	  go to 12
end if
 
	storage_current = storage_pre*0.6

!	errel = 0.001

 	  errel = 0.000001
      nvar = 1
      itmax = 5000
      icrit = 0

    x1=storage_current
    x2 = storage_pre*1.1+2 
    xacc=1.0E-06

!10	call DNEQNF(et_function,errel,nvar,itmax,storage_current,x,fnorm)

    x=rtsec(x1,x2,xacc, alpha, beta)

       
!      itype =N1rty(1)
      
!	if(icrit.eq.1)stop
	    
!if(itype.eq.4)then
!        write(*,*)'Current Storage',storage_pre
!		write(*,*)'Current Flow',current_flow
!		write(*,*)'Current Release',current_release
!		write(*,*)'Evaporation Rate',et_rate
!        write(*,*)'SOLVE FOR THE ABOVE VALUES USING MATHCAD AND ENTER &
!			THE END OF THE MONTH STORAGE'
	
!		read(*,*)x
!		goto 12
!end if

12     continue  

! Cases for when the storage is greater than the maximum, less than the minimum
! or in between, respectively.
if(x.ge.storage_max)then
		storage_current = storage_max
		temp = 0.5*(storage_current+storage_pre)
		evapo_current = et_cal(et_rate,temp, alpha, beta)
		spill = current_flow - current_release + storage_pre - storage_max &
     	- evapo_current
		deficit = 0.0
end if 

if(x.le.0.0)then 
		storage_current = 0.0
		spill = 0.0
		temp = 0.5*(storage_current+storage_pre)
		evapo_current = et_cal(et_rate,temp, alpha, beta)
		deficit = current_release - current_flow  - storage_pre + evapo_current
 end if 

 if((x.lt.storage_max).and.(x.gt.0.0))then
		
		storage_current = x
		spill = 0.0
		temp = 0.5*(storage_current+storage_pre)
		evapo_current = et_cal(et_rate,temp, alpha, beta)
		deficit = 0.0

  end if 
  ! check mass balance
       check1 = current_flow +storage_pre +deficit - spill - &
				current_release - evapo_current - storage_current

      if(iwrite.eq.1)write(42,14)check1

14   format(F14.4)

return
end

subroutine evap_solve(storage_pre,et_rate,alpha,current_flow,current_release,x)
implicit doubleprecision(a-h,o-z)

	temp1 = et_rate*alpha/2.0
	temp = current_flow-current_release


	temp2 = (storage_pre*(1-temp1) + temp)/(1+temp1)

	x = temp2

return
end



subroutine et_function(storage_current,fnorm,nvar, alpha, beta)
implicit doubleprecision(a-h,o-z)

common/et_est/et_rate,storage_pre,current_flow,current_release,bal_net, iwrite

    temp = (storage_pre+storage_current)/2.0

	tmp = current_flow  - current_release + storage_pre - storage_current 

	temp1 = et_cal(et_rate,temp, alpha, beta)

	fnorm = tmp - temp1

return

end


double precision function et_cal(et_rate,storage, alpha, beta)
implicit doubleprecision(a-h,o-z)

!	alpha = 0.3376373
!	beta = 0.842674
      
    et_cal = et_rate*alpha*(storage**beta)

return
end



subroutine boot_strap(q,qobs, nyr, nensem,ntime)
! Use NUmerical_libraries
implicit doubleprecision(a-h,o-z)

double precision q(ntime,nensem),qobs(ntime,nyr)

Do i = 1,nensem
    call random_number(rand)
	unif = 1+ (nyr-1)*rand
	index = nint(unif)

	Do j = 1,ntime

		 q(j,i) = qobs(j,index)
          index = index

	end do


end do


return

end

Subroutine reduce_Q(nsam,ntime,Q,ncv,Qcal,nred,nind)
implicit real*8(a-h,o-z)

Dimension Q(ntime,nsam),Qval(ntime,ncv),Qcal(ntime,nred) 

  j = 1

   k=1

Do i = 1,nsam

	  if((i.ge.nind).and.(i.lt.nind+ncv))then

				Do k1 = 1,ntime

					Qval(k1,k)= Q(k1,i)

				end do

				k = k+1

	   else 

				Do k1 = 1,ntime

	  				Qcal(k1,j) = Q(k1,i)

				end do 
			
				j  = j+1
		  
		end if

end do


return

end


! Secant method for root finding
    double precision FUNCTION rtsec(x1,x2,xacc, alpha, beta)
      implicit real*8 (a-h,o-z)
      INTEGER MAXIT
      PARAMETER (MAXIT=30)
      INTEGER j
      REAL*8 dx,f,fl,swap,xl

      call et_function(x1,fl,1, alpha, beta)
      call et_function(x2,f,1, alpha, beta)
      if(dabs(fl).lt.dabs(f))then
        rtsec=x1
        xl=x2
        swap=fl
        fl=f
        f=swap
      else
        xl=x1
        rtsec=x2
      endif
      do 11 j=1,MAXIT
        dx=(xl-rtsec)*f/(f-fl)
        xl=rtsec
        fl=f
        rtsec=rtsec+dx
      call et_function(rtsec,f,1, alpha, beta)

        if(dabs(dx).lt.xacc.or.f.eq.0.)then
    rtsec = rtsec
        return
        endif
11    continue
!      pause 'rtsec exceed maximum iterations'
      END


subroutine array_ini(ntime,arr, assigned_value)
implicit doubleprecision(a-h,o-z)

double precision arr(ntime),assigned_value

 Do i = 1, ntime
	  arr(i) = assigned_value
end do
	

return
end				

subroutine array_ini_two(ndim1,ndim2, arr, assigned_value)
implicit doubleprecision(a-h,o-z)

doubleprecision arr(ndim1,ndim2),assigned_value

Do j = 1,ndim1

	Do i = 1, ndim2
	  arr(j,i) = assigned_value
	end do

end do
	

return
end				


subroutine array_int_ini(ntime,arr,my_value)
Integer arr(ntime)

Do i = 1, ntime
	  arr(i) = my_value
end do
	

return
end				







