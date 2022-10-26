/*-----------------------------------------------------------------------
 * AdalineNeuralTLC.java
 *
 * Written by Steven Wyckoff, 2006
 *
 * This TLC was writen as part of a project for CS152 Neural Networks and
 * Soft Computing class at Harvey Mudd College under professor Keller,
 * fall, 2006. The intention was to implement a neural network to 
 * approximate the gain function in a Q-learning algorithm. Overall I 
 * think it works quite well, the major limitation being the reward
 * and inputs given.
 *
 * Be aware that the XMLSerializable implementation doesn't actually
 * do anything so if you're going around saving things you'll have to
 * fix it.
 *
 * see www.cs.hmc.edu/~swyckoff/neural/neural.html for more information.
 */

/*-----------------------------------------------------------------------
 * Copyright (C) 2001 Green Light District Team, Utrecht University 
 *
 * This program (Green Light District) is free software.
 * You may redistribute it and/or modify it under the terms
 * of the GNU General Public License as published by
 * the Free Software Foundation (version 2 or later).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty
 * of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 * See the documentation of Green Light District for further information.
 *------------------------------------------------------------------------*/


package gld.algo.tlc;

import gld.*;
import gld.sim.*;
import gld.algo.tlc.*;
import gld.infra.*;
import gld.utils.*;
import gld.xml.*;

import java.io.IOException;
import java.util.Random;
import java.util.*;
import java.awt.Point;
  
class Adaline
{
    public float[] m_weights;
    public int m_num_inputs;
    
    public Adaline(int num_inputs, Random rand)
    {
        m_num_inputs = num_inputs;
        //num_inputs+1 because we need to have a bias
        m_weights = new float[m_num_inputs+1];
        for(int i = 0; i < m_num_inputs+1; i++)
        {
            m_weights[i] = rand.nextFloat();
        }
    }
    
    public Adaline(float[] weights)
    {
        m_weights = weights;
        m_num_inputs = m_weights.length-1;
    }
    
    public float[] GetWeights()
    {
        
        return m_weights;
    }
    
    public void Train(float[] input, float desired, float learning_rate)
    {
        float raw = Evaluate(input);
        
        float error = desired - raw;

        float learning_factor = error * learning_rate;
        AdjustWeights(learning_factor, input);
    }
    
    public float Evaluate(float[] inputs)
    {
        float accum = 0.0f;
        for(int i = 0; i < m_num_inputs; i++)
        {
            accum += inputs[i] * m_weights[i];
        }
        //add in a bias
        accum += m_weights[m_num_inputs];
        return accum;
    }
    
    protected void AdjustWeights(float lrate, float[] errors)
    {
        for(int x = 0; x < m_num_inputs; x++)
        {
            m_weights[x] += lrate * errors[x];
        }
        //remember, bias
        m_weights[m_num_inputs] += lrate;
    }
}

class NeuralNode
{
    protected final static int green_index=0, red_index=1;
    
    Adaline[][] m_lane_q_functions;
    
    Node m_thisNode;
    Sign[] m_incomingSigns;
    int m_node_id;
    int m_num_lanes;
    int m_num_inputs;
    
    float m_gamma;
    float m_learning_rate;
    float m_reward_share;
    
    float[] m_reward_this_time;
    
    float[] m_gains;
    float[] m_previous_gains;
    float[] m_current_state;
    float[] m_previous_state;
    
    public NeuralNode(Infrastructure infra, int id, int num_inputs, float gamma, float learning, float reward_share, Random rand)
    {
        m_node_id = id;
        m_gamma = gamma;
        m_learning_rate = learning;
        m_reward_share = reward_share;
        
        Node[] nodes = infra.getAllNodes();
        m_thisNode = nodes[m_node_id];
        
        if (m_thisNode.getType() == Node.JUNCTION)
        {
            m_incomingSigns = ((Junction)m_thisNode).getSigns();
        }
        else
        {
            m_incomingSigns = new Sign[0];
        }
        
        m_num_lanes = m_incomingSigns.length;
          
        m_gains = new float[m_num_lanes];
        m_previous_gains = new float[m_num_lanes];
        
        m_lane_q_functions = new Adaline[m_num_lanes][];
        
        m_reward_this_time = new float[m_num_lanes];
        
        for(int i = 0; i < m_num_lanes; i++)
        {
            m_gains[i] = 0.0f;
            m_previous_gains[i] = 0.0f;
            
            m_lane_q_functions[i] = new Adaline[2];
            m_lane_q_functions[i][0] = new Adaline(num_inputs,rand);
            m_lane_q_functions[i][1] = new Adaline(num_inputs,rand);
            m_reward_this_time[i] = 0.0f;
        }
        
        m_previous_state = null;
        m_current_state = null;
       
    }
    
    public void GrantReward(float reward, int tlId)
    {
        //give the lane the full reward
        //and the rest a discounted one
        for(int i = 0; i < m_num_lanes; i++)
        {
            if(m_incomingSigns[i].getId() == tlId)
            {
                m_reward_this_time[i] += reward;
            }
            else
            {
                m_reward_this_time[i] += reward * m_reward_share;
            }
        }
    }
    
    //CalcGains will store the old state, update the gains and then train the network
    public void CalcGains(float[] newState)
    {
        m_previous_state = m_current_state;
        m_current_state = newState;
        
        for(int lane = 0; lane < m_num_lanes; lane++)
        {
            m_previous_gains[lane] = m_gains[lane];
            float red = m_lane_q_functions[lane][red_index].Evaluate(m_current_state);
            float green = m_lane_q_functions[lane][green_index].Evaluate(m_current_state);
            float dif = (green - red);
            m_gains[lane] = dif;
        }
        
        //this train must happen after calculating the new gains so that the reward
        //matches up properly with the stored previous state
        Train();
    }
    
    //need to take into accout whether we were green or red. It might be just another part of the state/input data
    //sign.MayDrive() is true if we were green, red otherwise. We'll need to extract that info when we update state
    public void Train()
    {
        if(m_previous_state == null)
        {
            return;
        }
        
        for(int i = 0; i < m_num_lanes; i++)
        {
            float nextq = m_gains[i];
            float reward_for_action = m_reward_this_time[i];
            float updated_q_value = reward_for_action + (m_gamma * nextq);
            boolean lane_was_green = m_incomingSigns[i].mayDrive();
            
            //System.out.println("lane "+i+" maydrive " + lane_was_green+" reward "+m_reward_this_time[i]+" next q "+nextq+" updated_q_value "+updated_q_value);
            if(lane_was_green)
            {
                m_lane_q_functions[i][green_index].Train(m_previous_state, updated_q_value, m_learning_rate);
            }
            else
            {
                m_lane_q_functions[i][red_index].Train(m_previous_state, updated_q_value, m_learning_rate);
            }
            
            //clean up our stuff
            m_reward_this_time[i] = 0.0f;            
        }
    }
    
    public float GetQ(int laneNumber)
    {
        return m_gains[laneNumber];
    }
    
    static float Max(float[] vals)
    {
        float max = Float.MIN_VALUE;
        float maxIndex = -1;
        for(int i = 0; i < vals.length; i++)
        {
            if(vals[i] >= max)
            {
                max = vals[i];
                maxIndex = i;
            }
        }
        return max;
    }
}

public class AdalineNeuralTLC extends TLController implements XMLSerializable,TwoStageLoader//,InstantiationAssistant
{      
    	protected final static String shortXMLName="tlc-adalineneural";
        
    	// TLC vars
	//protected Infrastructure infrastructure;
	protected TrafficLight[][] tls;
	protected Node[] allnodes;
	protected int num_nodes;
    
        protected int m_num_iters;
        
        protected static float[] RANDOM_CHANCE = {0.2f,0.03f};//A random gain setting is chosen instead of the on the TLC dictates with this chance
        protected static int RANDOM_THRESH = 500;//point at which it transitions to the next random chance
        protected static float PASS_REWARD = 100.0f;
        protected static float MOVE_REWARD = 5.0f;
        protected static float NOMOVE_REWARD = 0.0f;
        
        protected static float LEARNING_RATE = 0.01f;
        protected static float GAMMA_FACTOR = 0.5f;
        protected static float REWARD_SHARE = 0.1f;
        
	protected Random random;
	protected Infrastructure m_infrastructure;
        	
        
        protected NeuralNode[] m_node_q_functions;
        protected int m_num_inputs;
      
        
	/**
	 * Creates a new AdalineNeural Algorithm.
	 * This TLC-algorithm uses adalines to approximate the Q function for each state/action pair
	 * 
	 * @param i The infrastructure this algorithm will have to operate on
	 */
	public AdalineNeuralTLC(Infrastructure i)
	{	super(i);
		//assistant=this;
		random=new Random();
                m_num_iters = 0;
	}
	
	/**
	 * Changes the Infrastructure this algorithm is working on
	 * 
	 * @param i The new infrastructure for which the algorithm has to be set up
	 */
	public void setInfrastructure(Infrastructure infra)
        {
            random = new Random();
            m_num_iters = 0;
            
            super.setInfrastructure(infra);
            m_infrastructure = infra;
            try{
                
                Node[] nodes = m_infrastructure.getAllNodes();
                num_nodes = nodes.length;

                //the number of inputs gets set here
                m_num_inputs = 3 * m_infrastructure.getAllInboundLanes().size();
                
                m_node_q_functions = new NeuralNode[num_nodes];
                          
                for(int nodeId = 0; nodeId < num_nodes; nodeId++)
                {
                    int num_decisions = tld[nodeId].length;

                    //System.out.println("node: "+nodeId+" decisions: "+num_decisions+" num_inputs: "+m_num_inputs);
                    m_node_q_functions[nodeId] = new NeuralNode(m_infrastructure, nodeId, m_num_inputs, GAMMA_FACTOR, LEARNING_RATE, REWARD_SHARE, random);
                }

            }
            catch(Exception e)
            {
                System.out.println("Rats, problem with creating the AdalineNeuralTLC");
            }
	}
	
	
	/**
	 * Calculates how every traffic light should be switched
	 * @return Returns a double array of TLDecision's. These are tuples of a TrafficLight and the gain-value of when it's set to green.
	 * @see gld.algo.tlc.TLDecision
	 */	
	public TLDecision[][] decideTLs()
	{
      
            float[] input_set = new float[m_num_inputs];

            int input_index = 0;
            
            //Roaduser ru;
            //ListIterator queue;      
            
            int num_lanes;
	
            Drivelane d;
            
            for (int i=0; i < num_nodes; i++)
            {
                num_lanes = tld[i].length;
                for(int j=0; j < num_lanes; j++)
                {
                    d = tld[i][j].getTL().getLane();
                    int num_waiting = d.getNumRoadusersWaiting();
                    int length = d.getLength();
                    input_set[input_index] = (float)num_waiting / (float)length;
                    input_index++;
                    
                    input_set[input_index] = (float)num_waiting;
                    input_index++;
                    
                    if(d.isFull())
                        input_set[input_index] = 0.0f;
                    else
                        input_set[input_index] = 1.0f;
                    input_index++;
                    
                    /*
                     * This doesn't do any good because there is no connection to any reward right now
                    queue = d.getQueue().listIterator();
                    float ti = 0.0f;
				
                    // For each waiting Roaduser
                    for(int k = 0; k < num_waiting; k++) 
                    {
                            ru = (Roaduser) queue.next();
                            int timehere = ru.getDrivelaneStartTime() - m_infrastructure.getCurCycle();
                            ti += timehere;
                    }
                    
                    if(num_waiting > 0)
                    {
                        ti /= num_waiting;
                    }
                    
                    input_set[input_index] = ti;
                    input_index++;
                    **/
                }
            }
                
            
            //calculate the TL's' from the current state for each node using our network
            for(int i = 0; i < num_nodes; i++)
            {
                m_node_q_functions[i].CalcGains(input_set);
            }
            
            //Determine wheter it should be random or not
            m_num_iters++;
            float current_random_chance = 0.0f;
            if(m_num_iters > RANDOM_THRESH)
            {
               current_random_chance = RANDOM_CHANCE[1];
            }
            else
            {
                current_random_chance = RANDOM_CHANCE[0];
            }
             
            boolean randomrun = false;
            if (random.nextFloat() < current_random_chance)
            {
                randomrun = true;
            }
            
            //now lets fill in our TL table for the outside world
            for(int i = 0; i < num_nodes; i++)
            {
                int num_tl = tld[i].length;
                
                //this inner loop is to calculate the gain for each lane being green
                for(int j = 0; j < num_tl; j++)
                {
                    float q = m_node_q_functions[i].GetQ(j);

                    if(trackNode!=-1)
                    {
                        if(i==trackNode)
                        {
                            Drivelane currentlane = tld[i][j].getTL().getLane();
                            boolean[] targets = currentlane.getTargets();
                            System.out.println("node: "+i+" light: "+j+" gain: "+q+" "+targets[0]+" "+targets[1]+" "+targets[2]+" "+currentlane.getNumRoadusersWaiting());
                        }
                    }
                                
                    // If this is a random run, set all gains randomly
                    if(randomrun) 
                    {
                        q = random.nextFloat();
                    }
                    
                    tld[i][j].setGain(q);
                }
            }
            
            return tld;	
	}

	/**
	 * Provides the TLC-algorithm with information about a roaduser that has had it's go in the moveAllRoaduser-cycle.
	 * From this information it can be distilled whether a roaduser has moved or had to wait.
         *
         * This information will be used to figure out if there was a reward for the previous action.
	 * 
	 * @param _ru
	 * @param _prevlane
	 * @param _prevsign
	 * @param _prevpos
	 * @param _dlanenow
	 * @param _signnow
	 * @param _posnow
	 * @param _possiblelanes
	 * @param _ranges
	 */
	public void updateRoaduserMove(Roaduser _ru, Drivelane _prevlane, Sign _prevsign, int _prevpos, Drivelane _dlanenow, Sign _signnow, int _posnow, PosMov[] posMovs, Drivelane _desired)
	{
            
		// Roaduser has just left the building!
		if(_dlanenow == null || _signnow == null)
			return;
		
		if(_prevsign.getType()==Sign.TRAFFICLIGHT && 
                        (_signnow.getType()==Sign.TRAFFICLIGHT || _signnow.getType()==Sign.NO_SIGN))
                {
                    int tlId = _prevsign.getId();
                    int nodeId = _prevsign.getNode().getId();
                    float reward = 0.0f;

                    if(_prevsign == _signnow && _prevpos == _posnow) 
                    {      
                        //no move, no rerward
                        reward = NOMOVE_REWARD;
                    }
                    else if(_prevsign != _signnow)
                    {
                        //pass a sign, very good
                        reward = PASS_REWARD;
                    }
                    else 
                    {
                        //not as good to move but not pass a sign
                        reward = MOVE_REWARD;
                    }                       
                    
                    m_node_q_functions[nodeId].GrantReward(reward,tlId);
                    
                }

                
                
	}
	

	
	// XMLSerializable implementation
	// NOTICE: THIS IS NOT IMPLEMENTED IN ANY REAL FORM
	public void load (XMLElement myElement,XMLLoader loader) throws XMLTreeException,IOException,XMLInvalidInputException
	{	
            //NUM_STEPS=myElement.getAttribute("num-steps-s").getIntValue();
            //CROSSOVER_CHANCE=myElement.getAttribute("cross-prob").getFloatValue();
            //MUTATION_CHANCE=myElement.getAttribute("mut-prob").getFloatValue();
            //NUMBER_INDIVIDUALS=myElement.getAttribute("num-ind").getIntValue();
            //NUMBER_GENERATIONS=myElement.getAttribute("num-gen").getIntValue();
            num_nodes=myElement.getAttribute("num-nodes").getIntValue();
            //num_step=myElement.getAttribute("num-steps-o").getIntValue();
            //num_wait=myElement.getAttribute("num-wait").getIntValue();
            //num_move=myElement.getAttribute("num-move").getIntValue();
            //ind=(GenNeuralIndividual[])XMLArray.loadArray(this,loader,assistant);
            //pop=new BpropNeuralPopulation();
            //loader.load(this,pop);
	}

	public XMLElement saveSelf () throws XMLCannotSaveException
	{ 	
            XMLElement result=super.saveSelf();
            result.setName(shortXMLName);
            //result.addAttribute(new XMLAttribute("num-steps-s",NUM_STEPS));
            //result.addAttribute(new XMLAttribute("cross-pob",CROSSOVER_CHANCE));
            //result.addAttribute(new XMLAttribute("mut-prob",MUTATION_CHANCE));
            //result.addAttribute(new XMLAttribute("num-ind",NUMBER_INDIVIDUALS));
            //result.addAttribute(new XMLAttribute("num-gen",NUMBER_GENERATIONS));
            result.addAttribute(new XMLAttribute("num-nodes",num_nodes));
            //result.addAttribute(new XMLAttribute("num-steps-o",num_step));
            //result.addAttribute(new XMLAttribute("num-wait",num_wait));
            //result.addAttribute(new XMLAttribute("num-move",num_move));
            return result;
	}
  
	public void saveChilds (XMLSaver saver) throws XMLTreeException,IOException,XMLCannotSaveException
	{ 	
            //XMLArray.saveArray(ind,this,saver,"members");
            //saver.saveObject(pop);
	}

	public String getXMLName ()
	{ 	
            return "model."+shortXMLName;
	}	
	
	// TwoStageLoader implementation
	public void loadSecondStage (Dictionary dictionaries) throws XMLInvalidInputException,XMLTreeException
	{	
            super.loadSecondStage(dictionaries);
            //pop.loadSecondStage(dictionaries);
            //XMLUtils.loadSecondStage(new ArrayEnumeration(ind),dictionaries);
	}
	
	// Settings dialog

	public void showSettings(Controller c)
	{
		String[] descs = {"Threshold",
                                  "Reward for a car passing an intersection",
                                  "Reward for a car moving on a road",
                                  "Shared reward",
                                  "Gamma factor",
                                  "Learning Rate",
                                  "Random Chance 1", "Random Chance 2"};

                
                int[] ints = {RANDOM_THRESH};//, RANDOM_THRESH[1], RANDOM_THRESH[2]};
		float[] floats = {PASS_REWARD, MOVE_REWARD, REWARD_SHARE, GAMMA_FACTOR, LEARNING_RATE,
                                  RANDOM_CHANCE[0], RANDOM_CHANCE[1]};
                
		TLCSettings settings = new TLCSettings(descs, ints, floats);
		
		settings = doSettingsDialog(c, settings);

		RANDOM_THRESH = settings.ints[0];
                
                PASS_REWARD = settings.floats[0];
		MOVE_REWARD = settings.floats[1];
		REWARD_SHARE = settings.floats[2];
                GAMMA_FACTOR = settings.floats[3];
                LEARNING_RATE = settings.floats[4];
                RANDOM_CHANCE[0] = settings.floats[5];
                RANDOM_CHANCE[1] = settings.floats[6]; 
                
		setInfrastructure(m_infrastructure);
	}
 
}