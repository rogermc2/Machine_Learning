--  Based on scikit-learn/sklearn/neural_network/_stochastic_optimizers.py

with IL_Types; use IL_Types;

package Stochastic_Optimizers is

   type LR_Schedule_Type is (Constant_LR_Schedule, Adaptive_LR_Schedule,
                             Invscaling_LR_Schedule);
   --  Constant is a constant learning rate given by learning_rate_init.
   --  Invscaling gradually decreases the learning rate at each time step using
   --  an inverse scaling exponent of power_t.
   --  learning_rate = learning_rate_init / pow(t, power_t)
   --  Adaptive keeps the learning rate constant to learning_rate_init as long
   --  as the training keeps decreasing.
   --  Each time two consecutive epochs fail to decrease the training loss by
   --  tol or fail to increase the validation score by tol if 'early_stopping'
   --  is on, the current learning rate is divided by five.

   type Base_Optimizer is record
      Initial_Learning_Rate : Float := 0.1;
      Learning_Rate         : Float := 0.1;
   end record;

   type Adam_Optimizer is record
      Params                : Float_List;  --  coefs + intercepts
      Initial_Learning_Rate : Float := 0.1;
      Learning_Rate         : Float := 0.1;
      Beta_1                : Float := 0.9;
      Beta_2                : Float := 0.999;
      Epsilon               : Float := 10.0 ** (-8);
      Time_Step             : Integer;
      First_Moments         : Float_List;  --  ms
      Second_Moments        : Float_List;  --  vs
   end record;

   type SGD_Optimizer is record
      Params                : Float_List; --  coefs + intercepts
      Initial_Learning_Rate : Float := 0.1;
      Learning_Rate         : Float := 0.1;
      LR_Schedule           : LR_Schedule_Type := Constant_LR_Schedule;
      Momentum              : Float := 0.9;
      Use_Nesterov          : Boolean := True;
      Power_T               : Float := 0.5;
      Velocities            : Float_List;
   end record;

   procedure C_Init (Self                  : out Adam_Optimizer;
                     Params                : Float_List;
                     Initial_Learning_Rate : Float := 0.1;
                     Beta_1                : Float := 0.9;
                     Beta_2                : Float := 0.999;
                     Epsilon               : Float);
   procedure C_Init (Self                  : out SGD_Optimizer;
                     Params                : Float_List;
                     Initial_Learning_Rate : Float := 0.1;
                     LR_Schedule           : LR_Schedule_Type := Constant_LR_Schedule;
                     Momentum              : Float := 0.9;
                     Use_Nesterov          : Boolean := True;
                     Power_T               : Float := 0.5);

end Stochastic_Optimizers;
