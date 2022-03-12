--  Based on scikit-learn/sklearn/neural_network/_stochastic_optimizers.py

package body Stochastic_Optimizers is

   procedure C_Init (Self                  : out Adam_Optimizer;
                     Params                : Float_List;
                     Initial_Learning_Rate : Float := 0.1;
                     Beta_1                : Float := 0.9;
                     Beta_2                : Float := 0.999;
                     Epsilon               : Float) is
   begin
      Self.Params := Params;
      Self.Initial_Learning_Rate := Initial_Learning_Rate;
      Self.Beta_1 := Beta_1;
      Self.Beta_2 := Beta_2;
      Self.Epsilon := Epsilon;
   end C_Init;

   --  -------------------------------------------------------------------------

   procedure C_Init (Self                  : out SGD_Optimizer;
                     Params                : Float_List;
                     Initial_Learning_Rate : Float := 0.1;
                     LR_Schedule           : LR_Schedule_Type := Constant_LR_Schedule;
                     Momentum              : Float := 0.9;
                     Use_Nesterov          : Boolean := True;
                     Power_T               : Float := 0.5) is
   begin
      Self.Params := Params;
      Self.Initial_Learning_Rate := Initial_Learning_Rate;
      Self.LR_Schedule := LR_Schedule;
      Self.Momentum := Momentum;
      Self.Use_Nesterov := Use_Nesterov;
      Self.Power_T := Power_T;
   end C_Init;

   --  -------------------------------------------------------------------------

end Stochastic_Optimizers;
