
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Base_Neural;
with Label;
with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Printing;
with Stochastic_Optimizers;

--  Test_Gradient makes sure that the activation functions and their
--  derivatives are correct
procedure Test_Gradient is
   use Real_Float_Arrays;
   use Multilayer_Perceptron;
   use Stochastic_Optimizers;

   Routine_Name       : constant String := "Test_Gradient ";
   Eps                : constant Float := 10.0 ** (-5);
   Num_Samples        : constant Positive := 5;
   Num_Features       : constant Positive := 10;
   Hidden_Layer_Sizes : constant Positive := 10;
   X                  : Real_Float_Matrix (1 .. Num_Samples, 1 .. Num_Features);
   Y                  : Integer_Array (1 .. Num_Samples);
   Y2                 : Integer_Matrix (1 .. Num_Samples, 1 .. 1);
   LB                 : Label.Label_Binarizer;
   Layer_Sizes        : NL_Types.Integer_List;
   aClassifier        : MLP_Classifier;
   Activations        : Real_Matrix_List;
   Deltas             : Real_Matrix_List;
   Layer_Units        : NL_Types.Integer_List;
   Fan_In             : Positive;
   Fan_Out            : Positive;
   Params             : Parameters_List;
   Theta              : Parameters_List;
   Loss               : Float;
   pragma Unreferenced (Loss);

   function Loss_Grad_Function (Self      : in out MLP_Classifier;
                                Params    : Parameters_List;
                                Y         : Boolean_Matrix;
                                Gradients : out Parameters_List)
                                return Float is
   begin
      return Loss_Grad_LBFGS (Self, Params, X, Y, Activations, Gradients);

   end Loss_Grad_Function;

begin
   Put_Line (Routine_Name);
   Layer_Sizes.Append (Hidden_Layer_Sizes);
   for sample in X'Range loop
      for feature in X'Range (2) loop
         X (sample, feature) := abs (Maths.Random_Float);
      end loop;
   end loop;

   Layer_Units.Append (Num_Features);
   Layer_Units.Append (Hidden_Layer_Sizes);

   for num_labels in 2 .. 3 loop
      for value in Y'Range loop
         Y (value) := value mod num_labels + 1;
         Y2 (value, 1) := Y (value);
      end loop;

      --  L196
      for activ_type in Base_Neural.Activation_Type'Range loop
         aClassifier := C_Init
           (Activation => activ_type,
            Hidden_Layer_Sizes => Layer_Sizes,
            Solver => Stochastic_Optimizers.Lbfgs_Solver,
            Alpha => 10.0 ** (-5), Learning_Rate_Init => 0.2,
            Random_State => 1, Max_Iter => 1);
         Init_Optimizer (aClassifier);
         --  L206
         Fit (aClassifier, X, Y2);
         Layer_Units.Append (aClassifier.Attributes.N_Outputs);
         Theta := aClassifier.Attributes.Params;

         Activations.Clear;
         Deltas.Clear;
         Params.Clear;
         --  L217
         Activations.Append (X);

         for layer in 1 .. aClassifier.Attributes.N_Layers - 1 loop
            Activations.Append (Zero_Matrix (X'Length, layer + 1));
            Deltas.Append (Zero_Matrix (X'Length, layer + 1));
            Put_Line (Routine_Name & "L222 layer" & Integer'Image (layer));
            Fan_In := Layer_Units (layer);
            Fan_Out := Layer_Units (layer + 1);

            --  L224
            declare
               Param_Rec : Parameters_Record (Fan_In, Fan_Out);
            begin
               Params.Clear;
               Param_Rec.Coeff_Gradients := Zero_Matrix (Fan_In, Fan_Out);
               Param_Rec.Intercept_Grads := Zero_Array (Fan_Out);
               Params.Append (Param_Rec);
               Params := aClassifier.Attributes.Params;
            end;
         end loop;

         Put_Line (Routine_Name & "L233");
         --  L233
         declare
            Y_Bin        : constant Boolean_Matrix :=
                             Label.Fit_Transform (LB, Y);
            Theta_Length : constant Positive := Positive (Theta.Length);
            Eye          : constant Real_Float_Matrix :=
                             Unit_Matrix (Theta_Length);
            Num_Grad     : Parameters_List;
            dTheata      : Real_Vector (1 .. Theta_Length);
            Theta_P      : Parameters_List;
            Theta_M      : Parameters_List;
         begin
            Loss := Loss_Grad_Function (aClassifier, Theta, Y_Bin, Params);
            --  L239 numerically compute the gradients
            Put_Line (Routine_Name & "L239");
            for index in 1 .. Theta_Length loop
               for e_row in 1 .. Theta_Length loop
                  dTheata (e_row) := Eye (e_row, index) * Eps;
               end loop;

               declare
                  use Parameters_Package;
                  Theta_Rec_P : Parameters_Record (Theta (index).Num_Rows,
                                                   Theta (index).Num_Cols);
                  Theta_Rec_M : Parameters_Record (Theta (index).Num_Rows,
                                                   Theta (index).Num_Cols);
               begin
                  for t_index in Theta.First_Index .. Theta.Last_Index loop
                     Theta_Rec_P := Theta (t_index);
                     Theta_Rec_M := Theta_Rec_P;
                     Theta_Rec_P.Coeff_Gradients :=
                       Theta_Rec_P.Coeff_Gradients + dTheata;
                     Theta_Rec_P.Intercept_Grads :=
                       Theta_Rec_P.Intercept_Grads + dTheata;
                     Theta_P.Append (Theta_Rec_P);
                     Theta_Rec_M.Coeff_Gradients :=
                       Theta_Rec_M.Coeff_Gradients - dTheata;
                     Theta_Rec_M.Intercept_Grads :=
                       Theta_Rec_M.Intercept_Grads - dTheata;
                     Theta_P.Append (Theta_Rec_M);
                  end loop;

                  Put_Line (Routine_Name & "242+");
                  --  L242
                  Loss := Loss_Grad_Function (Self => aClassifier ,
                                              Params => Theta_P, Y => Y_Bin,
                                              Gradients => Theta_P);
                  Loss := Loss_Grad_Function  (aClassifier, Theta_M, Y_Bin,
                                               Theta_M);
                  Num_Grad.Append ((Theta_P.Element (1) - Theta_M.Element (1)) /
                                   (2.0 * Eps));
               end;
            end loop;

            for index in Params.First_Index .. Params.Last_Index loop
               Printing.Print_Parameters ("Params", Params (index));
               Printing.Print_Parameters ("Num_Grad", Num_Grad (index));
            end loop;
         end;
      end loop;

   end loop;

end Test_Gradient;