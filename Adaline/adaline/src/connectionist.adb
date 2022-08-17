--  Based on https://github.com/PabloSaavedra/
--  Adaline-Neuronal-Network/blob/master/SCX1.PAS

with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Numerics.Float_Random;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Connectionist_Types; use Connectionist_Types;
with Training;

package body Connectionist is

   Float_Gen : Ada.Numerics.Float_Random.Generator;

   Learning_Rate    : Float;
   Sail_Learn       : Float := 0.025;
   Max_Error        : Float := 0.0001;  --  maximum allowed error
   --      Neuron        : Neural_Node_Class;
   Neural_Net       : Neural_Net_List;
   --    input_patterns : tpatrentr; { array of input patterns
   --    outputd  : toutput; { array of desired outputs
   Active_Pattern   : Boolean := False; -- Training pattern loaded
   Num_Data         : Natural := 0; --  number of data (neuron inputs, data per pattern }
   Num_Patterns     : Natural := 0;  --  number of training patterns }
   --
   --    fichpesos : tfichtxt; { file of weights }
   --    fileerror : tfichtxt; { file of errors }
   --    nfichpesos : string; { name of weights file }
   --    nficherror : string; { error file name }
   --
   Error_Recorded   : Boolean := False;  --  EMCs are recorded }
   Weights_Recorded : Boolean := False; --  successive weights are saved }
   --    key : char; { option menu key }
   Program_End      : Boolean := False;

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

      procedure Adjust_Weights (theAdaline    : in out Adaline_Class;
                                Input         : Real_Float_Vector;
                                Learning_Rate : Float) is
      begin
         null;

      end Adjust_Weights;

      --  ---------------------------------------------------------------------------------------------

      function Evaluate (theAdaline : Adaline_Class;
                         Input      : Real_Float_Vector) return Float is
      begin
         return 0.0;

      end Evaluate;

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

   end Connectionist;
