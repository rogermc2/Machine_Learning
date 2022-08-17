

with Ada.Numerics.Float_Random;

with Maths;

package body Adaline is

   type Neural_Node_Class is record
      Node_ID            : Positive;
      Num_Inputs         : Positive;
      Num_Lanes          : Positive;
      Gamma              : Float;
      Learning_Rate      : Float;
      Reward_Share       : Float;
      Reward_This_Time   : Float;
      Gains              : Float;
      Previous_Gains     : Float;
      Current_state      : Float;
      Previous_state     : Float;
   end record;


   Float_Gen : Ada.Numerics.Float_Random.Generator;

   procedure Adaline (Num_Inputs : Positive) is
      use Ada.Numerics.Float_Random;
      --  Num_Inputs + 1 to allow for a bias
      theAdaline : Adaline_Class (Num_Inputs + 1);
   begin
      theAdaline.Num_Inputs := Num_Inputs;
      for index in 1 .. Num_Inputs + 1 loop
         theAdaline.Weights (index) := Float (Random (Float_Gen));
      end loop;

   end Adaline;

   --  ---------------------------------------------------------------------------------------------

   procedure Adaline (Weights : Real_Float_Vector) is
      use Ada.Numerics.Float_Random;
      theAdaline : Adaline_Class (Weights'Length - 1);
   begin
      theAdaline.Num_Inputs := Weights'Length - 1;
      theAdaline.Weights := Weights;

   end Adaline;

   --  ---------------------------------------------------------------------------------------------

   procedure Adjust_Weights (theAdaline : in out Adaline_Class;
                             Input        : Real_Float_Vector;
                            Learning_Rate : Float) is
   begin
      null;

   end Adjust_Weights;

   --  ---------------------------------------------------------------------------------------------

   function Evaluate (theAdaline : Adaline_Class;
                      Input : Real_Float_Vector) return Float is
   begin
      return 0.0;

   end Evaluate;

   --  ---------------------------------------------------------------------------------------------

--     procedure NeuralNode (Infrastructure infra, int id, int num_inputs, float gamma, float learning, float reward_share, Random rand)
   procedure NeuralNode (theNode        : in out Neural_Node_Class;
                         ID, Num_Inputs : Integer;
                         Gamma, Learning, Reward_Share : Float) is
     begin
        theNode.Node_ID := ID;
        theNode.Gamma := Gamma;
        theNode.Learning_Rate := Learning;
        theNode.Reward_Share := Reward_share;

--          Node[] nodes = infra.getAllNodes();
--          m_thisNode = nodes[m_node_id];

--          if (m_thisNode.getType() == Node.JUNCTION)
--          {
--              m_incomingSigns = ((Junction)m_thisNode).getSigns();
--          }
--          else
--          {
--              m_incomingSigns = new Sign[0];
--          }

--          theNode.Num_Lanes := m_incomingSigns.length;

--          theNode.Gains = new float[m_num_lanes];
--          theNode.Previous_Gains = new float[m_num_lanes];

--          m_lane_q_functions = new Adaline[m_num_lanes][];

--          theNode.Reward_This_Time = new float[m_num_lanes];

--          for(int i = 0; i < m_num_lanes; i++)
--          {
--              m_gains[i] = 0.0f;
--              m_previous_gains[i] = 0.0f;
--
--              m_lane_q_functions[i] = new Adaline[2];
--              m_lane_q_functions[i][0] = new Adaline(num_inputs,rand);
--              m_lane_q_functions[i][1] = new Adaline(num_inputs,rand);
--              m_reward_this_time[i] = 0.0f;
--          }

--          m_previous_state = null;
--          m_current_state = null;

   end NeuralNode;

   --  ---------------------------------------------------------------------------------------------

   procedure Run_Adaline is
   begin
      null;
   end Run_Adaline;

   --  ---------------------------------------------------------------------------------------------

   procedure Train (theAdaline    : in out Adaline_Class;
                    Input         : Real_Float_Vector; Desired,
                    Learning_Rate : Float) is
      Raw             : Float := Evaluate (theAdaline, input);
      Error           : Float := Desired - Raw;
      Learning_Factor : Float := Error * Learning_Rate;
   begin
      Adjust_Weights (theAdaline, Input, Learning_Rate);

   end Train;

   --  ---------------------------------------------------------------------------------------------

end Adaline;
