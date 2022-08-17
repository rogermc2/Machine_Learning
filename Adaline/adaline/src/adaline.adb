

with Ada.Numerics.Float_Random;

with Maths;

package body Adaline is

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
