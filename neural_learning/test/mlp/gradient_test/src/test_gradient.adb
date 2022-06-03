
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

    Routine_Name  : constant String := "Test_Gradient ";
    Num_Samples   : constant := 5;
    Num_Features  : constant := 10;
    X             : Real_Float_Matrix (1 .. Num_Samples, 1 .. Num_Features);
    Y             : Integer_Array (1 .. Num_Samples);
    Y2            : Integer_Matrix (1 .. Num_Samples, 1 .. 1);
    LB            : Label.Label_Binarizer;
    Y_Bin         : Boolean_Matrix (1 .. Num_Samples, 1 .. 1);
    Layer_Sizes   : NL_Types.Integer_List;
    aClassifier   : MLP_Classifier;
    Activations   : Real_Matrix_List;
    Deltas        : Real_Matrix_List;
    Layer_Units   : NL_Types.Integer_List;
    Fan_In        : Positive;
    Fan_Out       : Positive;
    Params        : Parameters_List;
    Theta         : Parameters_List;
    Loss          : Float;
    Num_Grads     : Positive;
    Theta_Length  : Positive;
    Coeff1_Test   : constant Real_Float_Matrix (1 .. 3, 1 .. 2) :=
                      ((0.098, 0.195756), (0.2956664, 0.096008),
                       (0.4939998, -0.002244));
    Coeff2_Test   : constant Real_Float_Matrix (1 .. 2, 1 .. 1) :=
                      ((1 => 0.04706), (1 => 0.154089));
    Coeff1_Error  : Real_Float_Matrix
      (Coeff1_Test'Range, Coeff1_Test'Range (2));
    Coeff2_Error  : Real_Float_Matrix
      (Coeff2_Test'Range, Coeff2_Test'Range (2));

    function Loss_Grad_Function (Self      : in out MLP_Classifier;
                                 Params    : Parameters_List;
                                 Y         : Boolean_Matrix;
                                 Gradients : out Parameters_List)
                                return Float is
    begin
        return Loss_Grad_LBFGS (Self, Params, X, Y, Activations, Gradients);

    end Loss_Grad_Function;

    function Zero_Array (Num_Rows : Positive)
                        return Real_Float_Vector is
        Loaded : Real_Float_Vector (1 .. Num_Rows);
    begin
        for row in Loaded'Range loop
            Loaded (row) := 0.0;
        end loop;

        return Loaded;

    end Zero_Array;

    function Zero_Matrix (Num_Rows, Num_Cols : Positive)
                         return Real_Float_Matrix is
        Loaded : Real_Float_Matrix (1 .. Num_Rows, 1 .. Num_Cols);
    begin
        for row in Loaded'Range loop
            for col in Loaded'Range (2) loop
                Loaded (row, col) := 0.0;
            end loop;
        end loop;

        return Loaded;

    end Zero_Matrix;

begin
    Put_Line (Routine_Name);
    Layer_Sizes.Append (10);
    for sample in X'Range loop
        for feature in X'Range (2) loop
            X (sample, feature) := abs (Maths.Random_Float);
        end loop;
    end loop;

    Layer_Units.Append (Num_Features);

    for num_labels in 2 .. 3 loop
        for value in Y'Range loop
            Y (value) := value mod num_labels + 1;
            Y2 (value, 1) := Y (value);
        end loop;
        Y_Bin := Label.Fit_Transform (LB, Y);

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
            Theta := aClassifier.Attributes.Params;

            Activations.Clear;
            Deltas.Clear;
            Params.Clear;
            --  L217
            Activations.Append (X);

            for layer in 1 .. aClassifier.Attributes.N_Layers loop
                Activations.Append (Zero_Matrix (X'Length, num_labels));
                Deltas.Append (Zero_Matrix (X'Length, num_labels));
                Fan_In := Layer_Units (num_labels);
                Fan_Out := Layer_Units (num_labels + 1);

                declare
                    Param_Rec : Parameters_Record (Fan_In, Fan_Out);
                    --           Y_Bin : Boolean_Matrix := Label.Fit_Transform (LB, Y);
                begin
                    Params.Clear;
                    Param_Rec.Coeff_Gradients := Zero_Matrix (Fan_In, Fan_Out);
                    Param_Rec.Intercept_Grads := Zero_Array (Fan_Out);
                    Params.Append (Param_Rec);
                    Params := aClassifier.Attributes.Params;
                end;
            end loop;

            --  L233
            Loss := Loss_Grad_Function (aClassifier, Theta, Y_Bin, Params);
            Theta_Length := Positive (Theta.Length);
            declare
                Num_Grad   : Real_Float_Vector := Zero_Array (Theta_Length);
                Eye        : Real_Float_Matrix := Unit_Matrix (Theta_Length);
                dTheata    : Real_Vector (1 .. Theta_Length);
                New_Theta  : Parameters_List;
--                  New_Theta1 : Parameters_Record ;
                Loss       : Float;
            begin
                for index in 1 .. Theta_Length loop
                    for e_row in 1 .. Theta_Length loop
                        dTheata (e_row) := Eye (e_row, index) * 10.0 ** (-5);
                    end loop;

                    for t_index in Theta.First_Index .. Theta.Last_Index loop
                        New_Theta.Append (Theta (t_index) + dTheata);
                    end loop;

                    Loss := Loss_Grad_Function
                      (Self => aClassifier ,
                       Params    => Theta,
                       Y         => Y_Bin,
                       Gradients => New_Theta);
--                      Num_Grad (index) := New_Theta1;
                end loop;
            end;
        end loop;

        Coeff1_Error :=
          abs (Coeff1_Test - Params.Element (1).Coeff_Gradients);
        Printing.Print_Float_Matrix_Formated ("Coeffs (1) errors", Coeff1_Error, 3);
        Coeff2_Error :=
          abs (Coeff2_Test - Params.Element (2).Coeff_Gradients);
        Printing.Print_Float_Matrix_Formated ("Coeffs (2) errors", Coeff2_Error, 3);
        --      Assert (Coeff1_Test = Params.Element (1).Coeff_Gradients,
        --              "Coeffs (1) Test failed");
        --      Assert (Coeff2_Test = Params.Element (2).Coeff_Gradients,
        --              "Coeffs (2) Test failed");
        --      Put_Line ("Coeffs tests passed");
    end loop;

end Test_Gradient;
