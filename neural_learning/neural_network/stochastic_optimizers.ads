--  Based on scikit-learn/sklearn/neural_network/_stochastic_optimizers.py

with NL_Types; use NL_Types;

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

   type Parameters_Record is record
      Coeff_Params     : Float_List_3D;
      Intercept_Params : Float_List_2D;
   end record;

   type Base_Optimizer is record
      Initial_Learning_Rate : Float := 0.1;
      Learning_Rate         : Float := 0.1;
   end record;

   type Adam_Optimizer is record
      --  The ith element of Coeff_Params represents the weight matrix
      --  corresponding to layer i.
      Coeff_Params             : Float_List_3D;
      --  The ith element of Intercept_Params represents the bias vector
      --  corresponding to layer i + 1.
      Intercept_Params         : Float_List_2D;
      Initial_Learning_Rate    : Float := 0.1;
      Learning_Rate            : Float := 0.1;
      Beta_1                   : Float := 0.9;
      Beta_2                   : Float := 0.999;
      Epsilon                  : Float := 10.0 ** (-8);
      Time_Step                : Integer;
      Coeff_First_Moments      : Float_List;  --  ms
      Coeff_Second_Moments     : Float_List;  --  vs
      Intercept_First_Moments  : Float_List;  --  ms
      Intercept_Second_Moments : Float_List;  --  vs
   end record;

   type SGD_Optimizer is record
      --  The ith element of Coeff_Params represents the weight matrix
      --  corresponding to layer i.
      Coeff_Params          : Float_List_3D;
      --  The ith element of Intercept_Params represents the bias vector
      --  corresponding to layer i + 1.
      Intercept_Params      : Float_List_2D;
      Initial_Learning_Rate : Float := 0.1;
      Learning_Rate         : Float := 0.1;
      Learning_Rate_Kind    : Learning_Rate_Type := Constant_Rate;
      LR_Schedule           : LR_Schedule_Type := Constant_LR_Schedule;
      Momentum              : Float := 0.9;
      Use_Nesterov          : Boolean := True;
      Power_T               : Float := 0.5;
      Velocities            : Parameters_Record;
   end record;

   type Optimizer_Record (Kind : Optimizer_Type := No_Optimizer) is record
      case Kind is
         when No_Optimizer => null;
         when Optimizer_Adam => Adam : Adam_Optimizer;
         when Optimizer_SGD => SGD   : SGD_Optimizer;
      end case;
   end record;

   procedure C_Init (Self                  : out Adam_Optimizer;
                     Coeff_Params          : Float_List_3D;
                     Intercept_Params      : Float_List_2D;
                     Initial_Learning_Rate : Float := 0.1;
                     Beta_1                : Float := 0.9;
                     Beta_2                : Float := 0.999;
                     Epsilon               : Float);
   procedure C_Init (Self                  : out SGD_Optimizer;
                     Coeff_Params          : Float_List_3D;
                     Intercept_Params      : Float_List_2D;
                     Initial_Learning_Rate : Float := 0.1;
                     Learning_Rate         : Float := 0.1;
                     Learning_Rate_Kind    : Learning_Rate_Type :=
                       Constant_Rate;
                     LR_Schedule           : LR_Schedule_Type :=
                       Constant_LR_Schedule;
                     Momentum              : Float := 0.9;
                     Use_Nesterov          : Boolean := True;
                     Power_T               : Float := 0.5);
   procedure Update_Params (Self   : in out Optimizer_Record;
                            Params : in out Parameters_Record;
                            Grads  : Parameters_Record);

end Stochastic_Optimizers;
