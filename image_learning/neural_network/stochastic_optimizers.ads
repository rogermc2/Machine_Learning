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

   type Optimizer_Type is (No_Optimizer, Optimizer_Adam, Optimizer_SGD);
   type Solver_Type is (Lbfgs_Solver, Sgd_Solver, Adam_Solver);
   type Learning_Rate_Type is (Constant_Rate, Invscaling_Rate, Adaptive_Rate);

   type Base_Optimizer is record
      Initial_Learning_Rate : Float := 0.1;
      Learning_Rate         : Float := 0.1;
   end record;

   type Adam_Optimizer is record
      Params                : Float_List_3D;  --  coefs + intercepts
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
      Params                : Float_List_3D; --  coefs + intercepts
      Initial_Learning_Rate : Float := 0.1;
      Learning_Rate         : Float := 0.1;
      Learning_Rate_Kind    : Learning_Rate_Type := Constant_Rate;
      LR_Schedule           : LR_Schedule_Type := Constant_LR_Schedule;
      Momentum              : Float := 0.9;
      Use_Nesterov          : Boolean := True;
      Power_T               : Float := 0.5;
      Velocities            : Float_List;
   end record;

   type Optimizer_Record (Kind : Optimizer_Type := No_Optimizer) is record
      case Kind is
         when No_Optimizer => null;
         when Optimizer_Adam => Adam : Adam_Optimizer;
         when Optimizer_SGD => SGD : SGD_Optimizer;
      end case;
   end record;

   procedure C_Init (Self                  : out Adam_Optimizer;
                     Params                : Float_List_3D;
                     Initial_Learning_Rate : Float := 0.1;
                     Beta_1                : Float := 0.9;
                     Beta_2                : Float := 0.999;
                     Epsilon               : Float);
   procedure C_Init (Self                  : out SGD_Optimizer;
                     Params                : Float_List_3D;
                     Initial_Learning_Rate : Float := 0.1;
                     LR_Schedule           : LR_Schedule_Type := Constant_LR_Schedule;
                     Momentum              : Float := 0.9;
                     Use_Nesterov          : Boolean := True;
                     Power_T               : Float := 0.5);
   function Get_Updates (Self  : in out Adam_Optimizer;
                         Grads : Float_List) return Float_List;
   function Get_Updates (Self  : in out SGD_Optimizer;
                         Grads : Float_List) return Float_List;

end Stochastic_Optimizers;