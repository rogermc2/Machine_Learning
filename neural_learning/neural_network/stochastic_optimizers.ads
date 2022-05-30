--  Based on scikit-learn/sklearn/neural_network/_stochastic_optimizers.py

with Ada.Containers.Indefinite_Vectors;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

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

   type Optimizer_Type is (No_Optimizer, Optimizer_Adam, Optimizer_Base, Optimizer_SGD);
   type Solver_Type is (Lbfgs_Solver, Sgd_Solver, Adam_Solver);
   type Learning_Rate_Type is (Constant_Rate, Invscaling_Rate, Adaptive_Rate);

   type Parameters_Record (Num_Rows, Num_Cols : Positive) is record
      Coeff_Gradients : Real_Float_Matrix (1 .. Num_Rows, 1 .. Num_Cols);
      Intercept_Grads : Real_Float_Vector (1 .. Num_Cols);
   end record;
   function "*" (L : Float; R : Parameters_Record) return Parameters_Record;
   pragma Inline ("*");
   function "-" (L, R : Parameters_Record) return Parameters_Record;
   pragma Inline ("-");

   package Parameters_Package is new
     Ada.Containers.Indefinite_Vectors (Positive, Parameters_Record);
   subtype Parameters_List is Parameters_Package.Vector;
   subtype Moments_List is Parameters_Package.Vector;
   function "+" (L, R : Parameters_List) return Parameters_List;
   pragma Inline ("+");

   type Base_Optimizer is record
      Initial_Learning_Rate : Float := 0.1;
      Learning_Rate         : Float := 0.1;
   end record;

   type Adam_Optimizer is record
      --  The ith element of Coeff_Params represents the weight matrix
      --  corresponding to layer i.
      --  The ith element of Intercept_Params represents the bias vector
      --  corresponding to layer i + 1.
      Params                   : Parameters_List;
      Initial_Learning_Rate    : Float := 0.1;
      Learning_Rate            : Float := 0.1;
      Beta_1                   : Float := 0.9;
      Beta_2                   : Float := 0.999;
      Epsilon                  : Float := 10.0 ** (-8);
      Time_Step                : Integer;
      First_Moments            : Moments_List;  --  ms
      Second_Moments           : Moments_List;  --  vs
   end record;

   type SGD_Optimizer is record
      --  The ith element of Coeff_Params represents the weight matrix
      --  corresponding to layer i.
      --  The ith element of Intercept_Params represents the bias vector
      --  corresponding to layer i + 1.
      Params                : Parameters_List;
      Initial_Learning_Rate : Float := 0.1;
      Learning_Rate         : Float := 0.1;
      Learning_Rate_Kind    : Learning_Rate_Type := Constant_Rate;
      LR_Schedule           : LR_Schedule_Type := Constant_LR_Schedule;
      Momentum              : Float := 0.9;
      Use_Nesterov          : Boolean := True;
      Power_T               : Float := 0.5;
      Velocities            : Parameters_List;
   end record;

   type Optimizer_Record (Kind : Optimizer_Type := No_Optimizer) is record
      case Kind is
         when No_Optimizer => null;
         when Optimizer_Adam => Adam : Adam_Optimizer;
         when Optimizer_Base => Base : Base_Optimizer;
         when Optimizer_SGD => SGD   : SGD_Optimizer;
      end case;
   end record;

   procedure C_Init (Self                  : out Adam_Optimizer;
                     Params                : Parameters_List;
                     Initial_Learning_Rate : Float := 0.1;
                     Beta_1                : Float := 0.9;
                     Beta_2                : Float := 0.999;
                     Epsilon               : Float := 10.0 ** (-8));
   procedure C_Init (Self                  : out SGD_Optimizer;
                     Params                : Parameters_List;
                     Initial_Learning_Rate : Float := 0.1;
                     Learning_Rate         : Float := 0.1;
                     Learning_Rate_Kind    : Learning_Rate_Type :=
                       Constant_Rate;
                     LR_Schedule           : LR_Schedule_Type :=
                       Constant_LR_Schedule;
                     Momentum              : Float := 0.9;
                     Use_Nesterov          : Boolean := True;
                     Power_T               : Float := 0.5);

   procedure C_Init (Self                  : out Base_Optimizer;
                     Initial_Learning_Rate : Float := 0.1);
   function Square (Rec : Parameters_Record) return Parameters_Record;
   function Sqrt (Rec : Parameters_Record) return Parameters_Record;
   function Trigger_Stopping (Self    : in out Optimizer_Record; Msg : String;
                              Verbose : Boolean) return Boolean;
   procedure Update_Params (Self      : in out Optimizer_Record;
                            Params    : in out Parameters_List;
                            Gradients : Parameters_List);
   procedure Update_Params (Self      : in out Adam_Optimizer;
                            Params    : in out Parameters_List;
                            Gradients : Parameters_List);
   procedure Update_Params (Self      : in out SGD_Optimizer;
                            Params    : in out Parameters_List;
                            Gradients : Parameters_List);

end Stochastic_Optimizers;
