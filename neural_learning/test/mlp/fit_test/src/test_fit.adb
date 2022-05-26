
--  with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Neural;
with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
--  with Printing;
with Stochastic_Optimizers;

procedure Test_Fit is
   --  use Ada.Containers;
   use Multilayer_Perceptron;
   use Stochastic_Optimizers;
   Routine_Name : constant String := "Test_Fit ";
   X            : Real_Float_Matrix (1 .. 1, 1 .. 3);
   Y            : Integer_Matrix (1 .. 1, 1 .. 1);
   Layer_Sizes  : NL_Types.Integer_List;
   Gradients    : Parameters_List;
   Grad_Record  : Parameters_Record (2, 1);
   Params       : Parameters_Record (1, 2);
   Velocities   : Parameters_List;
   aClassifier  : MLP_Classifier;

   procedure Set_Weights (Self : in out MLP_Classifier) is
   begin
      Params.Coeff_Gradients (1, 1) := 0.0;
      Params.Coeff_Gradients (1, 2) := 0.0;
      Params.Intercept_Grads (1) := 0.0;
      --  Params length is num layers - 1
      Self.Attributes.Params.Append (Params);
      Self.Attributes.Params.Append (Params);
   end Set_Weights;

begin
   Put_Line (Routine_Name);
   X (1, 1) := 0.6;
   X (1, 2) := 0.8;
   X (1, 3) := 0.7;
   Y (1, 1) := 0;
   Layer_Sizes.Append (2);
   aClassifier := C_Init
     (Solver => Stochastic_Optimizers.Sgd_Solver, Learning_Rate_Init => 0.1,
      Alpha => 0.1, Activation => Base_Neural.Logistic_Activation,
      Random_State => 1, Max_Iter => 1, Hidden_Layer_Sizes => Layer_Sizes,
      Momentum => 0.0);

   Set_Weights (aClassifier);
   aClassifier.Attributes.N_Outputs := 1;
   aClassifier.Attributes.N_Features := 3;
   aClassifier.Attributes.N_Iter := 0;
   aClassifier.Parameters.Learning_Rate := 0.1;
   aClassifier.Attributes.N_Layers := 3;
   Grad_Record.Coeff_Gradients (1, 1) := 0.0;
   Grad_Record.Coeff_Gradients (2, 1) := 0.0;
   Grad_Record.Intercept_Grads (1) := 0.0;
   Grad_Record.Intercept_Grads (2) := 0.0;
   Gradients.Append (Grad_Record);
   aClassifier.Attributes.Out_Activation := Base_Neural.Logistic_Activation;
   aClassifier.Attributes.T := 0;
   aClassifier.Attributes.Best_Loss := Float'Last;
   aClassifier.Attributes.No_Improvement_Count := 0;
   aClassifier.Parameters.Solver := Sgd_Solver;

   Init_Optimizer (aClassifier);

   Put_Line (Routine_Name & "Params size" & Integer'Image (Params.Num_Rows) & " x" &
               Integer'Image (Params.Num_Cols));
   Put_Line (Routine_Name & "Params.Coeff_Gradients size" &
               Integer'Image (Params.Coeff_Gradients'Length) & " x" &
               Integer'Image (Params.Coeff_Gradients'Length (2)));
   Put_Line (Routine_Name & "Params.Intercept_Grads size" &
               Integer'Image (Params.Intercept_Grads'Length));
   Velocities.Append (Params);
   aClassifier.Attributes.Optimizer.SGD.Power_T := 0.0;

   --  Partial_Fit updates the model with a single iteration over the data.
   Partial_Fit (aClassifier, X, Y);

end Test_Fit;
