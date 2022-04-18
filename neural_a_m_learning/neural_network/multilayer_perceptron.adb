--  Based on scikit-learn/sklearn/neural_network/_multilayer_perceptron.py

--  The simplest representation of a neural network is a Multilayer Perceptron.
--  In its simplest form a MLP is just three layers.
--  An input layer represented by a real matrix X (N×d) where N is the number
--  of training examples and d is the number of features.
--  A hidden layer which is usually a ReLU or a logistic sigmoid function.
--  Hidden layer i could be a ReLU function represented by
--  h_i(x) = ReLU (x) = max (x, 0)
--  In other words, if the input to the ReLU function is negative, the function
--  outputs a 0.
--  If the inputs x are positive, the ReLU function will output x.
--  The hidden layer feeds into the output layer which is just another function.
--  This function could be squared error function (in the context of regression)
--  or softmax (in the case of multiclass classification).
--  The MLP is complete when you consider the weight and bias matrices but
--  we don't need them for now.
--  The activation function is just what the name suggests; a function.
--  In the example above, the activation function for the hidden layer is the
--  ReLU function.
--  The activation function for the output layer was squared error or softmax.
--  the word "activations" in Machine Learning almost always refers to the
--- output of the activation function.
--  The possible activations in the hidden layer in the example above could
--  only be either 0 or 1.
--  Note that the hidden activations (output from the hidden layer) could
--  become input to other activation functions (in this case, the output layer
--  activation functions).
--  Pre-activation means the input to an activation function.

--  A Multilayer_Perceptron contains the attributes Coefs and Intercepts.
--  Coefs is a 3D list of weight matrices where the weight matrix at index i
--  represents the weights between layer i and layer i + 1.
--  Intercepts is a 2D list of bias vectors where the vector at index
--  the bias values added to layer i + 1.

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Calendar; use Ada.Calendar;
with Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;
--  with Utilities;

with Base_Mix;
with Data_Splitter;
--  with Encode_Utils;
with Label;
with Neural_Maths;
--  with Printing;
with Utils;

package body Multilayer_Perceptron is
--     pragma Warnings (Off);

    First_Pass : Boolean := True;

    procedure Compute_Loss_Gradient
      (Self        : MLP_Classifier;
       Layer       : Positive;
       Num_Samples : Positive;
       Activations : Matrix_List;
       Deltas      : Matrix_List;
       Grads       : in out Parameters_List);
    --     procedure Fit_Lbfgs (Self        : in out MLP_Classifier;
    --                          X           : Float_List_2D;
    --                          Y           : Integer_List_2D;
    --                          Activations  : Float_List_3D;
    --                          Deltas       : Float_List_3D;
    --                          Grads       : in out Parameters_List;
    --                          Layer_Units : Integer_List);
    procedure Fit_Stochastic (Self         : in out MLP_Classifier;
                              X            : Float_Matrix;
                              Y            : Integer_Matrix;
                              --                               Activations  : in out Matrix_List;
                              --                               Deltas       : out Matrix_List;
                              --                               Grads        : in out Parameters_List;
                              Incremental  : Boolean := False);
    procedure Forward_Pass (Self         : MLP_Classifier;
                            Activations  : in out Matrix_List);
    procedure Initialize (Self        : in out MLP_Classifier;
                          Layer_Units : NL_Types.Integer_List);
    function Init_Coeff (Self            : in out MLP_Classifier;
                         Fan_In, Fan_Out : Positive) return Parameters_Record;
    procedure Process_Batch (Self                     : in out MLP_Classifier;
                             X                        : Float_Matrix;
                             Y                        : Integer_Matrix;
                             Batch_Slice              : NL_Types.Slice_Record;
                             Batch_Size, Num_Features : Positive;
                             --                              Activations              : in out Matrix_List;
                             --                              Deltas                   : in out Matrix_List;
                             --                              Grads                    : in out Parameters_List;
                             Accumulated_Loss         : in out Float);
    procedure Update_Activations
      (Params             : Parameters_Record;
       Activations        : in out Matrix_List;
       Hidden_Activation  : Base_Neural.Activation_Type;
       Num_Layers, Layer  : Positive);
    procedure Update_No_Improvement_Count
      (Self  : in out MLP_Classifier; Early_Stopping : Boolean;
       X_Val : Float_Matrix);
    procedure Validate_Hyperparameters (Self : MLP_Classifier);
    --     procedure Validate_Input (Self               : in out MLP_Classifier;
    --                               Y                  : Integer_List);
    --                               Incremental, Reset : Boolean);

    --  -------------------------------------------------------------------------
    --  L241  Backprop computes the MLP loss function and its derivatives
    --       with respect to each parameter: weights and bias vectors.
    procedure Backprop (Self        : in out MLP_Classifier;
                        X           : Float_Matrix;
                        Y           : Integer_Matrix;
                        Activations : in out Matrix_List;
                        --                         Deltas      : out Matrix_List;
                        Loss        : out Float) is --;
        --                         Grads       : out Parameters_List) is
        use Ada.Containers;
        use Base_Neural;
        use NL_Types.Float_Package;
        Routine_Name       : constant String := "Multilayer_Perceptron.Backprop ";
        Num_Samples        : constant Positive := Positive (X'Length);
        Y_Float            : constant Float_Matrix := To_Float_Matrix (Y);
        Loss_Function_Name : Loss_Function;
        Deltas             : Matrix_List;
        Grads              : Parameters_List;
        Values             : Float := 0.0;
        Last               : Positive;
        --        Start_Time         : Time;
        --        End_Time           : Time;
        --        Derivative_Kind    : Derivative_Type;
        --        Inplace_Derivative : Float_List;
    begin
        --        Put_Line (Routine_Name & "Pre Forward_Pass, Activations size:" &
        --                    Count_Type'Image (Activations.Length));
        --        Put_Line (Routine_Name & "Pre Forward_Pass, Activations size:" &
        --                    Count_Type'Image (Activations (1).Length) & " x" &
        --                    Count_Type'Image (Activations (1) (1).Length));

        --  Forward_Pass elapsed time approx. 24 ms.
        Forward_Pass (Self, Activations);

        --  L284
        --        Start_Time := Clock;
        --  Loss computation elapsed time 400 ms
        if Self.Attributes.Loss_Function_Name = Log_Loss_Function and then
          Self.Attributes.Out_Activation = Logistic_Activation then
            Loss_Function_Name := Binary_Log_Loss_Function;
        end if;

        --        Printing.Print_Float_Lists_2D
        --              (Routine_Name & "Activations last layer", Activations.Last_Element);
        Assert (Y'Length = Activations.Last_Element'Length and
                  Y'Length (2) = Activations.Last_Element'Length (2),
                Routine_Name &
                  "L284 Activations.Last_Element size" &
                  Count_Type'Image (Activations.Last_Element'Length) &
                  " x" &
                  Count_Type'Image (Activations.Last_Element'Length (2)) &
                  " should be the same as Y size" & Integer'Image (Y'Length) &
                  " x" & Integer'Image (Y'Length (2)));

        case Loss_Function_Name is
            when Binary_Log_Loss_Function =>
                Loss := Binary_Log_Loss (Y, Activations.Last_Element);
            when Log_Loss_Function =>
                Loss := Log_Loss (Y, Activations.Last_Element);
            when Squared_Error_Function =>
                Loss := Squared_Loss (Y, Activations.Last_Element);
        end case;
        --        End_Time := Clock;
        --        Put_Line (Routine_Name & "Loss computation elapsed time: " &
        --                    Duration'Image ((End_Time -Start_Time) * 1000) & "mS");

        --  L289  Add L2 regularization term to loss
        --  L310 loop elapsed time 650 ms.
        for s in Self.Attributes.Params.First_Index ..
          Self.Attributes.Params.Last_Index loop
            declare
                S_List : constant Parameters_Record :=
                           Self.Attributes.Params (s);
                Ravel  : NL_Types.Float_List;
            begin
                for row in S_List.Coeff_Grads'Range loop
                    for col in S_List.Coeff_Grads'Range (2) loop
                        Ravel := Ravel & S_List.Coeff_Grads (row, col);
                    end loop;
                end loop;
                Values := Values + NL_Types.Dot (Ravel, Ravel);
            end;  --  declare
        end loop;

        --  L292
        Loss := Loss + 0.5 * Self.Parameters.Alpha * Values / Float (Num_Samples);
        Put_Line (Routine_Name & "L292 loss + L2 regularization : " &
                    Float'Image (Loss));

        --  L297 Backward propagate
        --  The calculation of delta[last]  works with the following combinations
        --  of output activation and loss function:
        --  sigmoid and binary cross entropy, softmax and categorical cross
        --  entropy and identity with squared loss.
        --  deltas : list, length = n_layers - 1
        --  The ith element of deltas holds the difference between the activations
        --  of the i + 1 layer and the backpropagated error.
        --  Deltas are gradients of loss with respect to z in each layer.
        --  The ith element of Activations (layers x values) is a list of values
        --  of the ith layer.

        --  L295  Backward propagate python last = self.n_layers_ - 2
        Last := Self.Attributes.N_Layers - 1;
        Deltas.Set_Length (Count_Type (Last));
        --        Assert (Y_Float.Length = Activations.Last_Element.Length,
        --                Routine_Name &  "L301 Y_Float length" &
        --                  Count_Type'Image (Y_Float.Length) &
        --                  " should equal Activations.Last_Element length" &
        --                  Count_Type'Image (Activations.Last_Element.Length));
        --  L301
        Deltas.Replace_Element (Deltas.Last_Index,
                                Activations.Last_Element - Y_Float);

        --  L304  Compute gradient for the last layer
        Compute_Loss_Gradient (Self, Last, Num_Samples, Activations, Deltas,
                               Grads);
        Put_Line (Routine_Name & "L310");

        --  L310, L308
        --  L310 loop elapsed time insignificant
        for index in reverse 2 .. Self.Attributes.N_Layers - 1 loop
            declare
                S_List : constant Parameters_Record :=
                           Self.Attributes.Params (index);
                Dot_L  : constant Float_Matrix := Deltas (index);
                Dot_R  : constant Float_Matrix := S_List.Coeff_Grads;
            begin
                Deltas (index - 1) :=
                  Dot (Dot_L, Transpose (Dot_R));

                case Self.Parameters.Activation is
                when Identity_Activation => null;
                when Logistic_Activation =>
                    Logistic_Derivative (Z => Activations (index),
                                         Del => Deltas (index - 1));
                when Tanh_Activation =>
                    Tanh_Derivative (Activations (index), Deltas (index - 1));
                when Relu_Activation =>
                    Relu_Derivative (Activations (index), Deltas (index - 1));
                when Softmax_Activation => null;
                end case;

                Compute_Loss_Gradient (Self, index - 1, Num_Samples, Activations,
                                       Deltas, Grads);
            end;  --  declare
        end loop;

    end Backprop;

    --  -------------------------------------------------------------------------
    --  L1054
    function C_Init (Hidden_Layer_Sizes  : NL_Types.Integer_List :=
                       NL_Types.Integer_Package.Empty_Vector;
                     Activation          : Base_Neural.Activation_Type :=
                       Base_Neural.Relu_Activation;
                     Solver              : Solver_Type := Adam_Solver;
                     Alpha               : Float := 0.0001;
                     Batch_Size          : Positive := 200;
                     Learning_Rate       : Float := 0.001;
                     Learning_Rate_Init  : Float := 0.001;
                     Power_T             : Float := 0.5;
                     Max_Iter            : Natural := 200;
                     Shuffle             : Boolean := True;
                     Random_State        : Natural := 0;
                     Tol                 : Float := 10.0 ** (-4);
                     Verbose             : Boolean := False;
                     Warm_Start          : Boolean := False;
                     Momentum            : Float := 0.9;
                     Nesterovs_Momentum  : Boolean := True;
                     Early_Stopping      : Boolean := False;
                     Validation_Fraction : Float := 0.1;
                     Beta_1              : Float := 0.9;
                     Beta_2              : Float := 0.999;
                     Epsilon             : Float := 10.0 ** (-8);
                     N_Iter_No_Change    : Natural := 10;
                     Max_Fun             : Natural := 15000) return MLP_Classifier is
        Classifier : MLP_Classifier;
    begin
        Classifier.Parameters.Hidden_Layer_Sizes  := Hidden_Layer_Sizes;
        Classifier.Parameters.Activation          := Activation;
        Classifier.Parameters.Solver              := Solver;
        Classifier.Parameters.Alpha               := Alpha;
        Classifier.Parameters.Batch_Size          := Batch_Size;
        Classifier.Parameters.Learning_Rate       := Learning_Rate;
        Classifier.Parameters.Learning_Rate_Init  := Learning_Rate_Init;
        Classifier.Parameters.Power_T             := Power_T;
        Classifier.Parameters.Max_Iter            := Max_Iter;
        Classifier.Parameters.Shuffle             := Shuffle;
        Classifier.Parameters.Random_State        := Random_State;
        Classifier.Parameters.Tol                 := Tol;
        Classifier.Parameters.Verbose             := Verbose;
        Classifier.Parameters.Warm_Start          := Warm_Start;
        Classifier.Parameters.Momentum            := Momentum;
        Classifier.Parameters.Nesterovs_Momentum  := Nesterovs_Momentum;
        Classifier.Parameters.Early_Stopping      := Early_Stopping;
        Classifier.Parameters.Validation_Fraction := Validation_Fraction;
        Classifier.Parameters.Beta_1              := Beta_1;
        Classifier.Parameters.Beta_2              := Beta_2;
        Classifier.Parameters.Epsilon             := Epsilon;
        Classifier.Parameters.N_Iter_No_Change    := N_Iter_No_Change;
        Classifier.Parameters.Max_Fun             := Max_Fun;
        First_Pass                                := True;
        return Classifier;
    end C_Init;

    --  -------------------------------------------------------------------------
    --  L177 Compute_Loss_Gradient does back-propagation for a specified layer
    --  by computing the gradient of loss with respect to the coefs and
    --  intercept for the layer.

    --  Coef_Grads is a 3D list of weight matrices where the weight matrix at
    --   index i represents the weights between layer i and layer i + 1.
    --  Intercept_Grads is a 2D list of bias vectors where the vector at index
    --  the bias values added to layer i + 1.
    procedure Compute_Loss_Gradient
      (Self        : MLP_Classifier;
       Layer       : Positive;
       Num_Samples : Positive;
       Activations : Matrix_List;
       Deltas      : Matrix_List;
       Grads       : in out Parameters_List) is
        use Ada.Containers;
        Routine_Name : constant String :=
                         "Multilayer_Perceptron.Compute_Loss_Gradient ";
        Delta_M      : constant Float_Matrix := Deltas (Layer);
        Activ_M      : constant Float_Matrix := Activations (Layer);
        Params       : constant Parameters_Record :=
                         Self.Attributes.Params (Layer);
        Coeffs       : constant Float_Matrix := Params.Coeff_Grads;
        Delta_Act    : Float_Matrix
          (Activ_M'First (2) .. Activ_M'Last (2),
           Delta_M'First (2) .. Delta_M'Last (2));
        --        Delta_Mean   : Float_Array (1 .. Params.Num_Rows);
        New_Grad     : Parameters_Record (Params.Num_Rows, Params.Num_Cols);
    begin
        Put_Line (Routine_Name & "layer:" & Integer'Image (layer));
        Put_Line (Routine_Name & "Num_Samples:" & Integer'Image (Num_Samples));
        --        Put_Line (Routine_Name & "Deltas (Layer) length" &
        --                    Count_Type'Image (Delta_M'Length));
        --        Put_Line (Routine_Name & "Activations (Layer) length" &
        --                    Count_Type'Image (Activ_M'Length));

        --  The ith element of Deltas holds the difference between the
        --  activations of the i + 1 layer and the backpropagated error.
        Assert (Deltas.Element (Layer)'Length =
                  Transpose (Activations.Element (Layer))'Length (2),
                Routine_Name & "Num rows" &
                  Integer'Image (Deltas.Element (Layer)'Length) &
                  " of right matrix doesn't equal num colums" &
                  Integer'Image
                  (Transpose (Activations.Element (Layer))'Length (2)) &
                  " of left matrix");
        --        Put_Line (Routine_Name & "Activations (Layer) cols" &
        --                    Count_Type'Image (Activations.Element (Layer)'Length (2));
        Put_Line (Routine_Name & "Deltas (Layer) length" &
                    Count_Type'Image (Deltas.Element (Layer)'Length) & " x" &
                    Count_Type'Image (Deltas.Element (Layer)'Length (2)));
        Delta_Act := Dot (Transpose (Activations (Layer)), Deltas (Layer));
--          Put_Line (Routine_Name & "Mean 1 length" &
--                      Count_Type'Image (Neural_Maths.Mean (Deltas (Layer), 1)'Length));
--          Put_Line (Routine_Name & "Mean 2 length" &
--                      Count_Type'Image (Neural_Maths.Mean (Deltas (Layer), 2)'Length));
        --  L188
        declare
            Delta_Mean : constant Float_Array := Neural_Maths.Mean (Deltas (Layer), 1);
        begin
            Put_Line (Routine_Name & "Delta_Mean initial length" &
                        Count_Type'Image (Delta_Mean'Length));
            if Grads.Is_Empty or else Grads.Length < Count_Type (Layer) then
                Put_Line (Routine_Name & "setting Grads length");
                Grads.Set_Length (Count_Type (Layer));
            end if;

            Assert (Delta_Act'Length = Coeffs'Length, Routine_Name &
                      "Delta_Act Length" & Count_Type'Image (Delta_Act'Length) &
                      " should equal Coeff_Grads length" &
                      Count_Type'Image (Coeffs'Length));

            --        Put_Line (Routine_Name & "Delta_Act rows" &
            --                    Count_Type'Image (Delta_Act'Length));
            --        Put_Line (Routine_Name & "Delta_Act cols" &
            --                    Count_Type'Image (Delta_Act'Length (2)));
            Put_Line (Routine_Name & "Coeffs size" &
                        Count_Type'Image (Coeffs'Length) & " x" &
                        Count_Type'Image (Coeffs'Length (2)));
            --  L185
            --  Grad.Coeff_Grads is a 2D list of fan_in x fan_out lists
            New_Grad.Coeff_Grads :=
              Delta_Act + Self.Parameters.Alpha * Coeffs;

            New_Grad.Coeff_Grads :=
              New_Grad.Coeff_Grads / Float (Num_Samples);

            Put_Line (Routine_Name & "L189");
            Put_Line (Routine_Name & "New_Grad.Intercept_Grads length" &
                        Count_Type'Image (New_Grad.Intercept_Grads'Length));
            Put_Line (Routine_Name & "Delta_Mean length" &
                        Count_Type'Image (Delta_Mean'Length));
            --  L189
            New_Grad.Intercept_Grads := Delta_Mean;
            Grads (layer) := New_Grad;
        end;  --  declare

    end  Compute_Loss_Gradient;

    --  -------------------------------------------------------------------------
    --  L377  BaseMultilayerPerceptron._Fit
    procedure Fit (Self        : in out MLP_Classifier;
                   X           : Float_Matrix;
                   Y           : Integer_Matrix;
                   Incremental : Boolean := False) is
        use Ada.Containers;
        Routine_Name       : constant String :=
                               "Multilayer_Perceptron.Fit ";
        Num_Features       : constant Positive := Positive (X'Length (2));
        --  The ith element of Activations holds the values of the ith layer.
        --        Activations        : Matrix_List;
        --        Deltas             : Matrix_List;
        Hidden_Layer_Sizes : constant NL_Types.Integer_List :=
                               Self.Parameters.Hidden_Layer_Sizes;
        Layer_Units        : NL_Types.Integer_List;
        --  Coef_Grads      layers * features * values
        --  Intercept_Grads layers * y values
        --        Grads              : Parameters_List;
    begin
        Validate_Hyperparameters (Self);
        First_Pass :=
          Self.Attributes.Params.Is_Empty or else
          (not Self.Parameters.Warm_Start and then not Incremental);

        --  L404
        --  layer_units = [n_features] + hidden_layer_sizes + [self.n_outputs_]
        Layer_Units.Append (Num_Features);
        if Hidden_Layer_Sizes.Length > 0 then
            for index in Hidden_Layer_Sizes.First_Index ..
              Hidden_Layer_Sizes.Last_Index loop
                Layer_Units.Append (Hidden_Layer_Sizes.Element (index));
            end loop;
        end if;
        Layer_Units.Append (Self.Attributes.N_Outputs);

        --  L409
        if First_Pass then
            Initialize (Self, Layer_Units);
        end if;

        --  if ? then
        --  L414 Set the Activation values of the first layer
        --        Activations.Append (X);  -- layer x samples x features
        --  end if;
        --  Deltas is a 3D list initialized by Backprop
        --  The ith element of Deltas holds the difference between the
        --  activations of the i + 1 layer and the backpropagated error.
        --        Deltas.Set_Length (Count_Type (Self.Attributes.N_Layers - 1));

        --  L417 Initialized grads are empty vectors, no initialization required.

        Put_Line (Routine_Name & "L427");
        --  L427
        if Self.Parameters.Solver = Sgd_Solver or else
          Self.Parameters.Solver = Adam_Solver then
            Fit_Stochastic (Self, X, Y, Incremental);
            --             (Self, X, Y, Deltas, Grads, Incremental);
        elsif Self.Parameters.Solver = Lbfgs_Solver then
            null;
            --           Fit_Lbfgs (Self, X, Y_2D, Activations, Deltas, Grads, Layer_Units);
        end if;

    end Fit;

    --  -------------------------------------------------------------------------
    --  L516
    --     procedure Fit_Lbfgs (Self            : in out MLP_Classifier;
    --                          X               : Float_List_2D;
    --                          Y               : Integer_List_2D;
    --                          Activations     : in out Float_List_3D;
    --                          Deltas          : in out Float_List_3D;
    --                          Grads           : in out Parameters_List;
    --                          Layer_Units     : Integer_List) is
    --        Routine_Name : constant String := "Multilayer_Perceptron.Fit_Lbfgs ";
    --        Num_Samples  : constant Positive := Positive (X.Length);
    --        Start        : Positive := 1;
    --        Last         : Positive;
    --        N_Fan_In     : Positive;
    --        N_Fan_Out    : Positive;
    --     begin
    --        Self.Attributes.Coef_Indptr.Clear;
    --        Self.Attributes.Intercept_Indptr.Clear;

    --  L524  Save sizes and indices of coefficients for faster unpacking
    --        for index in 1 .. Self.Attributes.N_Layers - 1 loop
    --           N_Fan_In := Layer_Units.Element (index);
    --           N_Fan_Out := Layer_Units.Element (index + 1);
    --           Last := Start + N_Fan_In * N_Fan_Out;
    --           Self.Attributes.Coef_Indptr.Append ((Start, Last,
    --                                               N_Fan_In, N_Fan_Out));
    --           Start := Last + 1;
    --        end loop;

    --  L532  Save sizes and indices of intercepts for faster unpacking
    --        Start := 1;
    --        for index in 1 .. Self.Attributes.N_Layers - 1 loop
    --           Last := Start + N_Fan_In * N_Fan_Out;
    --           Self.Attributes.Intercept_Indptr.Append ((Start, Last));
    --           Start := Last + 1;
    --        end loop;
    --
    --        Assert (False, Routine_Name & "coding incomplete.");
    --
    --     end Fit_Lbfgs;

    --  -------------------------------------------------------------------------

    --  L563
    procedure Fit_Stochastic (Self         : in out MLP_Classifier;
                              X            : Float_Matrix;
                              Y            : Integer_Matrix;
                              --                               Activations  : in out Matrix_List;
                              --                               Deltas       : out Matrix_List;
                              --                               Grads        : in out Parameters_List;
                              Incremental  : Boolean := False) is
        use Ada.Containers;
        use Estimator;
        Routine_Name           : constant String :=
                                   "Multilayer_Perceptron.Fit_Stochastic ";
        Is_Classifier          : constant Boolean :=
                                   Self.Estimator_Kind = Classifier_Estimator;
        Num_Samples            : constant Positive := Positive (X'Length);
        Num_Features           : constant Positive := Positive (X'Length (2));
        LE_U                   : Label.Label_Encoder
          (Label.Class_Unique, Num_Samples);
        Iter                   : Natural := 0;
        Continue               : Boolean := True;
        --  Activations: layers x samples x features
        Early_Stopping         : constant Boolean
          := Self.Parameters.Early_Stopping and then not Incremental;
        Test_Size              : constant Positive
          := Positive (Self.Parameters.Validation_Fraction * Float (Num_Samples));
        Train_Size             : constant Positive := Num_Samples - Test_Size;
        --  Stratify         : Integer_List_2D;
        --  Should_Stratify  : Boolean;
        Train_X                : Float_Matrix
          (1 .. Train_Size, 1 .. Num_Features);
        Train_Y                : Integer_Matrix (1 .. Train_Size, 1 .. 1);
        Test_X                 : Float_Matrix
          (1 .. Test_Size, 1 .. Num_Features);
        Test_Y                 : Integer_Matrix (1 .. Test_Size, 1 .. 1);
        Batch_Size             : Positive;
        Batches                : NL_Types.Slices_List;
        Accumulated_Loss       : Float := 0.0;
        Msg                    : Unbounded_String;
        Is_Stopping            : Boolean := False;
        --          Start_Time             : Time;
        --          End_Time               : Time;
        --          Batch_Start_Time       : Time;
        --          Batch_End_Time         : Time;
    begin
        --  L576
        if not Incremental or else
          Self.Attributes.Optimizer.Kind = No_Optimizer then
            case Self.Parameters.Solver is
            when Adam_Solver =>
                declare
                    Optimizer : Optimizer_Record (Optimizer_Adam);
                begin
                    Stochastic_Optimizers.C_Init
                      (Optimizer.Adam, Params => Self.Attributes.Params,
                       Initial_Learning_Rate =>
                         Self.Parameters.Learning_Rate_Init,
                       Beta_1 => Self.Parameters.Beta_1,
                       Beta_2 => Self.Parameters.Beta_2,
                       Epsilon => Self.Parameters.Epsilon);

                    Self.Attributes.Optimizer := Optimizer;
                end;

            when Lbfgs_Solver =>
                Put_Line (Routine_Name & "Lbfgs");
                declare
                    Optimizer : Optimizer_Record (Optimizer_SGD);
                begin
                    Stochastic_Optimizers.C_Init
                      (Optimizer.SGD, Params => Self.Attributes.Params,
                       Initial_Learning_Rate =>
                         Self.Parameters.Learning_Rate_Init,
                       Learning_Rate => Self.Parameters.Learning_Rate,
                       Momentum => Self.Parameters.Momentum,
                       Use_Nesterov => Self.Parameters.Nesterovs_Momentum,
                       Power_T => Self.Parameters.Power_T);

                    Self.Attributes.Optimizer := Optimizer;
                end;

            when Sgd_Solver =>
                Put_Line (Routine_Name & "SGD");
                declare
                    Optimizer : Optimizer_Record (Optimizer_SGD);
                begin
                    Stochastic_Optimizers.C_Init
                      (Optimizer.SGD, Params => Self.Attributes.Params,
                       Initial_Learning_Rate => Self.Parameters.Learning_Rate_Init,
                       Learning_Rate => Self.Parameters.Learning_Rate,
                       Momentum => Self.Parameters.Momentum,
                       Use_Nesterov => Self.Parameters.Nesterovs_Momentum,
                       Power_T => Self.Parameters.Power_T);

                    Self.Attributes.Optimizer := Optimizer;
                end;
            end case;
        end if;

        --  L597
        if Early_Stopping then
            --           Should_Stratify := Is_Classifier;
            --           if Should_Stratify then
            --              Stratify := Y;
            --           end if;

            Data_Splitter.Train_Test_Split
              (X => X, Y => Y,
               Train_Size => Train_Size, Test_Size  => Test_Size,
               Train_X => Train_X, Train_Y => Train_Y,
               Test_X  => Test_X, Test_Y => Test_Y);

            if Is_Classifier then
                Test_Y := Label.Inverse_Transform (LE_U, Test_Y);
            end if;
        end if;

        --  L617
        Batch_Size := Self.Parameters.Batch_Size;
        if Batch_Size = 0 then
            Batch_Size := Integer'Min (200, Num_Samples);
        elsif Batch_Size > Num_Samples then
            Put_Line (Routine_Name & "WARNING: Batch size" &
                        Integer'Image (Self.Parameters.Batch_Size)  &
                        " clipped to" & Integer'Image (Num_Samples));
            Batch_Size := Num_Samples;
        end if;
        --          Put_Line (Routine_Name & "Batch size" & Integer'Image (Batch_Size));

        --  L628
        Put_Line (Routine_Name & "Batch_Size: " &
                    Integer'Image (Batch_Size));
        while Continue and then iter < Self.Parameters.Max_Iter loop
            --  approx. 5 sec per iteration .
            --                  Start_Time := Clock;
            iter := iter + 1;
            if Self.Parameters.Shuffle then
                null;
            end if;

            Accumulated_Loss := 0.0;
            --  Batches is a list of slice lists
            Batches := Utils.Gen_Batches (Num_Samples, Batch_Size);
            --  L636
            --  Batch loop time 5.5 sec
            --                  Batch_Start_Time := Clock;
            Put_Line (Routine_Name & "Batches size" &
                        Count_Type'Image (Batches.Length));
            for batch_index in Batches.First_Index ..
              Batches.Last_Index loop
                --  Time per batch loop iteration 200ms
                Process_Batch (Self, X, Y, Batches (Batch_Index), Batch_Size,
                               Num_Features, Accumulated_Loss);
                --                             Num_Features, Deltas, Grads, Accumulated_Loss);
            end loop;
            --                  Batch_End_Time := Clock;
            --                  Put_Line (Routine_Name & "Batch loop time: " &
            --                              Duration'Image
            --                              ((Batch_End_Time - Batch_Start_Time) * 1000) & "mS");

            --  L661
            Self.Attributes.N_Iter := Self.Attributes.N_Iter + 1;
            Self.Attributes.Loss := Accumulated_Loss / Float (Num_Samples);
            Self.Attributes.T := Self.Attributes.T + Num_Samples;
            Self.Attributes.Loss_Curve.Append (Self.Attributes.Loss);
            --  L666
            if Self.Parameters.Verbose then
                Put_Line (Routine_Name & "Iteration" &
                            Integer'Image (Self.Attributes.N_Iter) &
                            "loss = " & Float'Image (Self.Attributes.Loss));
            end if;

            --  L669 Update no_improvement_count based on training loss or
            --       validation score according to early_stopping
            Update_No_Improvement_Count (Self, Early_Stopping, Test_X);
            --  for learning rate that needs to be updated at iteration end;
            if Self.Attributes.Optimizer.Kind = Optimizer_SGD then
                null;
            end if;

            --  676
            if Self.Attributes.No_Improvement_Count >
              Self.Parameters.N_Iter_No_Change then
                if Early_Stopping then
                    Msg := Msg & To_Unbounded_String (Routine_Name & "Validation score");
                else
                    Msg := Msg & To_Unbounded_String (Routine_Name & "Training loss");
                end if;
                Msg := Msg & To_Unbounded_String (" did not improve more than tol =" &
                                                    Float'Image (Self.Parameters.Tol) & " for" &
                                                    Integer'Image (Self.Parameters.N_Iter_No_Change) &
                                                    " consecutive epochs.");
                --  L692
                Is_Stopping :=
                  Trigger_Stopping (Self.Attributes.Optimizer, To_String (Msg),
                                    Self.Parameters.Verbose);
                Continue := not Is_Stopping;
                if Continue then
                    Self.Attributes.No_Improvement_Count := 0;
                end if;
            end if;

            Continue := Continue and not Incremental;
            if Continue and
              Self.Attributes.N_Iter = Self.Parameters.Max_Iter then
                Put_Line (Routine_Name &  "Stochastic Optimizer:");
                Put_Line ("    Maximum iterations (" &
                            Integer'Image (Self.Parameters.Max_Iter) &
                            ") reached but the optimization hasn't " &
                            "converged yet.");
                New_Line;
            end if;

            --                  End_Time := Clock;
            --                  Put_Line (Routine_Name & "Iter loop time: " &
            --                              Duration'Image ((End_Time - Start_Time) * 1000) & "mS");
            Put_Line (Routine_Name & "L710 Accumulated_Loss: " &
                        Float'Image (Accumulated_Loss));
        end loop;

        --  L711
        if Early_Stopping then
            Self.Attributes.Params := Self.Parameters.Best_Params;
        end if;

    end Fit_Stochastic;

    --  -------------------------------------------------------------------------
    --  L119  Forward_Pass performs a forward pass on the neural network by
    --  computing the values of the neurons in the hidden layers and the
    --  output layer.
    procedure Forward_Pass (Self         : MLP_Classifier;
                            Activations  : in out Matrix_List) is
    --  The ith element of Activations (length n_layers - 1) holds the values
    --  of the ith layer.
    --  Activations: layers x samples x features
        use Ada.Containers;
        use Base_Neural;
        Routine_Name       : constant String :=
                               "Multilayer_Perceptron.Forward_Pass ";
        Hidden_Activation  : constant Activation_Type :=
                               Self.Parameters.Activation;
        Output_Activation  : constant Activation_Type :=
                               Self.Attributes.Out_Activation;
        Num_Layers         : constant Positive := Self.Attributes.N_Layers;
        Params             : constant Parameters_List := Self.Attributes.Params;
        --        Start_Time         : constant Time := Clock;
        --        End_Time           : Time;
        --  Elapsed time approx. 24 ms.
    begin
        --  Iterate over the hidden layers
        --  The Python range(stop) function returns a sequence of numbers,
        --   starting from 0 by default, incrementing by 1 (by default) and
        --   stopping BEFORE "stop". That is, at "stop" - 1.
        --  Therefore range(self.n_layers_ - 1): range is
        --            first (0) .. last (n_layers_ - 2)
        Put_Line (Routine_Name & "L119 Activations length:" &
                    Count_Type'Image (Activations.Length));
        --  L119
        for layer in 1 .. Num_Layers - 1 loop
            declare
                Activation : constant Float_Matrix := Activations (layer);
            begin
                Put_Line (Routine_Name & "Activations (" & Integer'Image (layer) &
                            ") length:" & Count_Type'Image (Activation'Length) &
                            " x" & Count_Type'Image (Activation'Length (2)));
            end;
            Put_Line (Routine_Name & "Coefficient_Matrix size:" &
                        Count_Type'Image
                        (Params.Element (layer).Coeff_Grads'Length) & " x" &
                        Count_Type'Image
                        (Params.Element (layer).Coeff_Grads'Length (2)));
            Update_Activations (Params (layer), Activations, Hidden_Activation,
                                Num_Layers, layer);
        end loop;

        --  L138 For the last layer
        case Output_Activation is
            when Identity_Activation => null;
            when Logistic_Activation => Logistic (Activations (Num_Layers));
            when Tanh_Activation => Tanh (Activations (Num_Layers));
            when Relu_Activation => Relu (Activations (Num_Layers));
            when Softmax_Activation => Softmax (Activations (Num_Layers));
        end case;

        --        End_Time := Clock;
        --        Put_Line (Routine_Name & "elapsed time: " &
        --                    Duration'Image ((Start_Time - End_Time) * 1000) & "mS");

        Put_Line (Routine_Name & "Activations length:" &
                    Count_Type'Image (Activations.Length));
    end Forward_Pass;

    --  -------------------------------------------------------------------------

    --  L360  BaseMultilayerPerceptron._init_coef
    function Init_Coeff (Self            : in out MLP_Classifier;
                         Fan_In, Fan_Out : Positive) return Parameters_Record is
        use Maths;
        use Float_Math_Functions;
        use Base_Neural;
        --        Routine_Name : constant String := "Multilayer_Perceptron.Init_Coeff ";
        Params       : Parameters_Record (Fan_In, Fan_Out);
        Factor       : Float;
        Init_Bound   : Float;
    begin
        if Self.Parameters.Activation = Logistic_Activation then
            Factor := 2.0;
        else
            Factor := 6.0;
        end if;

        Init_Bound := Sqrt (Factor / Float (Fan_In + Fan_Out));
        --  Generate random weights
        for f_in in 1 .. Fan_In loop
            for f_out in 1 .. Fan_Out loop
                Params.Coeff_Grads (f_in, f_out) := Init_Bound * Random_Float;
            end loop;
        end loop;

        --  Generate random bias
        for f_out in 1 .. Fan_Out loop
            Params.Intercept_Grads (f_out) := Init_Bound * Random_Float;
        end loop;

        return Params;

    end Init_Coeff;

    --  ------------------------------------------------------------------------

    --  L320
    procedure Initialize (Self        : in out MLP_Classifier;
                          Layer_Units : NL_Types.Integer_List) is
    --        use Ada.Containers;
        use Base_Neural;
        --        Routine_Name   : constant String := "Multilayer_Perceptron.Initialize ";
        Fan_In  : Positive;
        Fan_Out : Positive;
    begin
        Self.Attributes.N_Iter := 0;
        Self.Attributes.T := 0;
        Self.Attributes.N_Layers := Natural (Layer_Units.Length);
        Self.Attributes.Out_Activation := Logistic_Activation;
        Self.Attributes.Params.Clear;

        --  L344
        for layer in 1 .. Self.Attributes.N_Layers - 1 loop
            --  Add coefficent matrices and intercept vectors for layer.
            Fan_In := Layer_Units (layer);
            Fan_Out := Layer_Units (layer + 1);
            Self.Attributes.Params.Append (Init_Coeff (Self, Fan_In, Fan_Out));
        end loop;

        --  L351
        if Self.Parameters.Solver = Sgd_Solver or else
          Self.Parameters.Solver = Adam_Solver then
            Self.Attributes.Loss_Curve.Clear;
            Self.Attributes.No_Improvement_Count := 0;
            if Self.Parameters.Early_Stopping then
                Self.Parameters.Validation_Scores.Clear;
                Self.Parameters.Best_Validation_Score := Float'Safe_First;
            else
                Self.Attributes.Best_Loss := Float'Safe_Last;
            end if;
        end if;

    end Initialize;

    --  -------------------------------------------------------------------------

    procedure Process_Batch (Self                     : in out MLP_Classifier;
                             X                        : Float_Matrix;
                             Y                        : Integer_Matrix;
                             Batch_Slice              : NL_Types.Slice_Record;
                             Batch_Size, Num_Features : Positive;
                             --                              Activations              : in out Matrix_List;
                             --                              Deltas                   : in out Matrix_List;
                             --                              Grads                    : in out Parameters_List;
                             Accumulated_Loss         : in out Float) is
    --        use Ada.Containers;
        Routine_Name : constant String := "Multilayer_Perceptron.Process_Batch ";
        X_Batch      : Float_Matrix (1 .. Batch_Size, 1 .. Num_Features);
        Y_Batch      : Integer_Matrix (1 .. Batch_Size, 1 .. 1);
        Activations  : Matrix_List;
        Grads        : Parameters_List;
        Batch_Row    : Positive;
        Batch_Loss   : Float;
    begin
        for row in Batch_Slice.First .. Batch_Slice.Last loop
            Batch_Row := row - Batch_Slice.First + 1;
            for col in 1 .. Num_Features loop
                X_Batch (Batch_Row, col) := X (row, col);
            end loop;
            Y_Batch (Batch_Row, 1) := Y (row, 1);
        end loop;

        Activations.Clear;
        --  L645
        --  Time per Backprop loop iteration 170ms
        Activations.Append (X_Batch);
        Backprop (Self, X_Batch, Y_Batch, Activations, Batch_Loss);
        --        Backprop (Self, X_Batch, Y_Batch, Activations, Deltas, Batch_Loss,
        --                  Grads);

        Accumulated_Loss := Accumulated_Loss + Batch_Loss *
          Float (Batch_Slice.Last - Batch_Slice.First + 1);
        Put_Line (Routine_Name & "L657 Accumulated_Loss: " &
                    Float'Image (Accumulated_Loss));
        --  L657 update weights
        Stochastic_Optimizers.Update_Params (Self.Attributes.Optimizer,
                                             Self.Attributes.Params, Grads);

    end Process_Batch;

    --  -------------------------------------------------------------------------

    procedure Update_Activations
      (Params             : Parameters_Record;
       Activations        : in out Matrix_List;
       Hidden_Activation  : Base_Neural.Activation_Type;
       Num_Layers, Layer  : Positive) is
        use Ada.Containers;
        use Base_Neural;
        Routine_Name         : constant String :=
                                 "Multilayer_Perceptron.Update_Activations ";
        Activ_Layer          : constant Float_Matrix := Activations (layer);
        Coefficient_Matrix   : constant Float_Matrix := Params.Coeff_Grads;
        Intercepts           : Float_Array := Params.Intercept_Grads;
        Activs_Dot_Coeffs    : constant Float_Matrix
          := Dot (Activ_Layer, Coefficient_Matrix);
        Activ_With_Intercept : Float_Matrix
          (1 .. Activ_Layer'Length, 1 .. Activs_Dot_Coeffs'Length (2));
    begin
        --              Put_Line (Routine_Name & "Acts_Dot_Coeffs:" &
        --                          Count_Type'Image (Activs_Dot_Coeffs'Length) & " x" &
        --                          Count_Type'Image (Activs_Dot_Coeffs'Length (2)));
        --  Dot (samples x features, samples x coefficients (weights))
        --  => samples x (coefficient * feature)
        --        if Integer (Activations.Length) <= layer then
        --           Activations.Set_Length (Count_Type (layer + 1));
        --        end if;

        --  L131
        --        Activations.Replace_Element (layer + 1, Activs_Dot_Coeffs);
        --  Add layer + 1
        Activations.Append (Activs_Dot_Coeffs);
        Put_Line (Routine_Name & "L131 Activations length:" &
                    Count_Type'Image (Activations.Length));
        declare
            Activ2 : constant Float_Matrix := Activations (layer + 1);
        begin
            Put_Line (Routine_Name & "Activations" &
                        Integer'Image (layer) & " length:" &
                        Count_Type'Image (Activ_Layer'Length) & " x" &
                        Count_Type'Image (Activ_Layer'Length (2)));
            Put_Line (Routine_Name & "Activations" &
                        Integer'Image (layer + 1) & " length:" &
                        Count_Type'Image (Activ2'Length) & " x" &
                        Count_Type'Image (Activ2'Length (2)));
        end;
        Put_Line (Routine_Name & "Activs_Dot_Coeffs length" &
                    Count_Type'Image (Activs_Dot_Coeffs'Length) & " x" &
                    Count_Type'Image (Activs_Dot_Coeffs'Length (2)));
        Put_Line (Routine_Name & "Act_With_Intercept length:" &
                    Count_Type'Image (Activ_With_Intercept'Length) & " x" &
                    Count_Type'Image (Activ_With_Intercept'Length (2)));

        --  L132
        Activ_With_Intercept := Activations (layer + 1);
        --  Intercepts: layers x intercept values
        Intercepts := Params.Intercept_Grads;
        --              Activations.Replace_Element (layer + 1, Activations (layer + 1) +
        --                                             Intercepts (layer));
        --              for intercept in Intercepts'Range loop
        for row in Activ_With_Intercept'Range loop
            --                 Put_Line (Routine_Name & "Activ_With_Intercept row:" &
            --                             Integer'Image (row));
            for col in Activ_With_Intercept'Range (2) loop
                Activ_With_Intercept (row, col) :=
                  Activ_With_Intercept (row, col) +
                  Intercepts (row);
            end loop;
        end loop;
        --              end loop;
        Activations (layer + 1) := Activ_With_Intercept;

        --  L134 For the hidden layers
        --           if layer + 1 /= Num_Layers - 1 then
        if layer + 1 /= Num_Layers then
            case Hidden_Activation is
            when Identity_Activation => null;
            when Logistic_Activation =>
                Logistic (Activations (layer + 1));
            when Tanh_Activation => Tanh (Activations (layer + 1));
            when Relu_Activation => Relu (Activations (layer + 1));
            when Softmax_Activation =>
                Softmax (Activations (layer + 1));
            end case;
        end if;
    end Update_Activations;

    --  -------------------------------------------------------------------------
    --  L716
    procedure Update_No_Improvement_Count
      (Self  : in out MLP_Classifier; Early_Stopping : Boolean;
       X_Val : Float_Matrix) is
        Routine_Name     : constant String :=
                             "Multilayer_Perceptron.Update_No_Improvement_Count ";
        Last_Valid_Score : Float;
        Score_Val        : Float;
    begin
        if Early_Stopping then
            Score_Val := Base_Mix.Score (X_Val);
            Self.Parameters.Validation_Scores.Append (Score_Val);
            Last_Valid_Score := Self.Parameters.Validation_Scores.Last_Element;
            if Self.Parameters.Verbose then
                Put_Line (Routine_Name & "Validation score: " &
                            Float'Image (Last_Valid_Score));
            end if;

            --  L728
            Put_Line (Routine_Name & "L728");
            if Last_Valid_Score <
              Self.Parameters.Best_Validation_Score + Self.Parameters.Tol then
                Self.Attributes.No_Improvement_Count :=
                  Self.Attributes.No_Improvement_Count + 1;
            else
                Self.Attributes.No_Improvement_Count := 0;
            end if;

            if Last_Valid_Score > Self.Parameters.Best_Validation_Score then
                Self.Parameters.Best_Validation_Score := Last_Valid_Score;
                Self.Parameters.Best_Params := Self.Attributes.Params;
            end if;

        else
            if Self.Attributes.Loss_Curve.Last_Element >
              Self.Attributes.Best_Loss then
                Self.Attributes.No_Improvement_Count :=
                  Self.Attributes.No_Improvement_Count + 1;
            else
                Self.Attributes.No_Improvement_Count := 0;
            end if;

            if Self.Attributes.Loss_Curve.Last_Element <
              Self.Attributes.Best_Loss then
                Self.Attributes.Best_Loss :=
                  Self.Attributes.Loss_Curve.Last_Element;
            end if;
        end if;

    end Update_No_Improvement_Count;

    --  -------------------------------------------------------------------------
    --  L455
    procedure Validate_Hyperparameters (Self : MLP_Classifier) is
    --                               Incremental, Reset : Boolean) is
    --        Routine_Name : constant String := "Multilayer_Perceptron.Validate_Hyperparameters ";
    begin
        null;

    end Validate_Hyperparameters;

    --  -------------------------------------------------------------------------

    --     procedure Validate_Input (Self               : in out MLP_Classifier;
    --                               --                               X                  : Float_List_2D;
    --                               Y                  : Integer_Array) is
    --                               Incremental, Reset : Boolean) is
    --        Routine_Name : constant String := "Multilayer_Perceptron.Validate_Input ";
    --     begin
    --        if Self.Attributes.Classes.Is_Empty and then
    --          Self.Parameters.Warm_Start then
    --           Self.Attributes.Classes := Encode_Utils.Unique (Y);
    --        else
    --           null;
    --        end if;
    --
    --     end Validate_Input;

    --  -------------------------------------------------------------------------

end Multilayer_Perceptron;
