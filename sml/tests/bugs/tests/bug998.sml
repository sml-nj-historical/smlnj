(* bug998.sml *)

signature MESSAGE_LANG =
    sig                              
               (* need more than two contructors in next *)
	datatype agent_id = UK | Kenya | Japan | World
	datatype commodity = Steel | Cars
        eqtype state_attribute
        datatype amount = Amount of int | Query
	datatype statement = State_info of agent_id * state_attribute * amount
	type message 
    end;

signature AGENT =
    sig
        structure Message_Lang : MESSAGE_LANG
	type agent 
	val update_beliefs : agent -> Message_Lang.statement list -> agent
    end;
    
signature AGENT_DYNAMICS =
    sig
	structure Agent : AGENT
        val process_message :
	      Agent.Message_Lang.message * Agent.agent -> Agent.agent 
	val core_functions : Agent.agent -> Agent.agent  
    end;


functor Neutral_Agent_Dynamics ( structure Agent : AGENT ) =
    struct
	structure Agent = Agent
        fun add_belief bel agt = agt
        fun process_message (_,a) = a
    end;


functor World_Agent_Dynamics (structure Agent : AGENT
			     ) : AGENT_DYNAMICS =
    struct
	structure Neutral_Dynamics =
	    Neutral_Agent_Dynamics( structure Agent = Agent )
	structure Agent = Agent
        open Agent
        open Message_Lang

          (* "Expand" is raised without next line  *)
	fun revise_beliefs agt bels = agt 

        exception NoStock of agent_id * commodity
        exception NoMoney of agent_id

        fun process_message (msg ,agt) = agt 

        fun show s = TextIO.output( TextIO.stdOut, s )
	    (* this needs def by cases *)
            (*    and no wildcard      *)
        fun showtr World = "World" 
	  | showtr UK    = "UK"
	  | showtr Japan = "Japan"
	  | showtr Kenya = "Kenya" (* wildcard here removes the bug *)
        fun show_neg_info ( NoStock ( agent_id, cdy ) ) =
	    show ( "Trader "^(showtr agent_id))
    	  | show_neg_info ( NoMoney agent_id ) =
	    show ( "Trader "^(showtr agent_id) )  


        fun update_stock_level trader agt =
            let val newC = Amount 0
                           handle SubAmt =>
			       ( show_neg_info(NoStock ( trader, Cars));
				       Amount 0 )
                      (* two decls needed here  *)
		val newS = Amount 0
		           handle SubAmt =>
			       ( show_neg_info(NoStock (trader, Steel));
				       Amount 0 )
	    in  agt end

                      (* this also needed *)
                      (*  -- identity function not enough *)
        fun core_functions a = update_stock_level Kenya a

    end;
