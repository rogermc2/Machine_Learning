with Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Base_Neural;
with Basic_Printing;        use Basic_Printing;
with Neural_Maths;
with Stochastic_Optimizers; use Stochastic_Optimizers;

package body Neural_Model is

   procedure Backward
     (aModel : in out Sequential_Model; Sample, L_Index : Positive);
   function Deactivate
     (aModel : in out Sequential_Model; Sample, L_Index : Positive)
      return Real_Float_Vector;
   function Deriv_ReLU (X : Real_Float_Vector) return Real_Float_Vector;
   function Deriv_Softmax (X : Real_Float_Vector) return Real_Float_Matrix;
   procedure Forward
     (aModel : in out Sequential_Model; Sample_Index : Positive);
   function To_Matrix (Data : Real_Float_Vector) return Real_Float_Matrix;
   procedure Update
     (Connection : in out Stochastic_Optimizers.Parameters_Record;
      aLayer     : in out Layer; Learn_Rate : Float);

   --  -------------------------------------------------------------------------

   procedure Add_Connections (aModel : in out Sequential_Model) is
      use Real_Float_Arrays;
      use Stochastic_Optimizers;
      Routine_Name : constant String := "Neural_Model.Add_Connections ";
   begin
      for layer in aModel.Layers.First_Index .. aModel.Layers.Last_Index - 1
      loop
         declare
            Connect :
            Parameters_Record
              (aModel.Layers (layer + 1).Num_Nodes,
               aModel.Layers (layer).Num_Nodes);
         begin
            for row in Connect.Coeff_Gradients'Range loop
               for col in Connect.Coeff_Gradients'Range (2) loop
                  --  Random_Float generates a random number in the range  -1.0 .. 1.0
                  Connect.Coeff_Gradients (row, col) :=
                    0.5 * abs (Maths.Random_Float);
               end loop;
               Connect.Intercept_Grads (row) := 0.5 * abs (Maths.Random_Float);
               --                 Print_Float_Matrix (Routine_Name & "Coeff_Gradients",
               --                                     Connect.Coeff_Gradients, 1, 2, 1, 2);
            end loop;

            aModel.Connections.Append (Connect);
         end;  --  declare block
      end loop;

   end Add_Connections;

   --  ---------------------------------------------------------------------------

   --  Initialization - add first layer
   procedure Add_First_Layer
     (aModel : in out Sequential_Model; Input_Data : Real_Float_Vector)
   is
      First_Layer : Layer (Input_Data'Length, Input_Data'Length);
   begin
      First_Layer.Input_Data := Input_Data;
      First_Layer.Nodes      := Input_Data;
      aModel.Layers.Append (First_Layer);

   end Add_First_Layer;

   --  ---------------------------------------------------------------------------

   procedure Add_Labels
     (aModel : in out Sequential_Model; Labels : Real_Float_Matrix)
   is
   begin
      aModel.Labels := Labels;

   end Add_Labels;

   --  ---------------------------------------------------------------------------
   --  Initialization - add other layers
   procedure Add_Layer
     (aModel     : in out Sequential_Model; Num_Nodes : Positive;
      Activation :        Activation_Kind := Identity_Activation)
   is
      use Real_Float_Arrays;
      Routine_Name : constant String := "Neural_Model.Add_Layer others  ";
      Prev_Layer   : constant Layer             := aModel.Layers.Last_Element;
      Prev_Nodes   : constant Real_Float_Vector := Prev_Layer.Nodes;
      thisLayer    : Layer (Prev_Layer.Num_Nodes, Num_Nodes);
   begin
      thisLayer.Input_Data := Prev_Nodes;
      thisLayer.Activation := Activation;
      aModel.Layers.Append (thisLayer);

   end Add_Layer;

   --  -------------------------------------------------------------------------

   procedure Back_Propogate
     (aModel    : in out Sequential_Model; Sample : Positive;
      Optimiser : Stochastic_Optimizers.Optimizer_Record) is
      use Stochastic_Optimizers;
      use Real_Float_Arrays;
      use Real_Matrix_List_Package;
      Routine_Name : constant String := "Neural_Model.Back_Propogate ";
   begin
      Put_Line (Routine_Name);
      for layer_id in reverse
        aModel.Layers.First_Index + 1 .. aModel.Layers.Last_Index loop
         Backward (aModel, sample, layer_id);
      end loop;

   end Back_Propogate;

   --  -------------------------------------------------------------------------

   procedure Backward
     (aModel : in out Sequential_Model; Sample, L_Index : Positive) is
      use Real_Float_Arrays;
      Routine_Name  : constant String := "Neural_Model.Backward ";
      This_Layer    : Layer := aModel.Layers (L_Index);
      Next_Layer    : Layer := aModel.Layers (L_Index - 1);
      dEdY          : constant Real_Float_Vector :=
                        Deactivate (aModel, Sample, L_Index);
      --  Transpose of Coeff_Gradients is dX/dY
      Input_Error   : constant Real_Float_Vector :=
                        Transpose
                          (aModel.Connections (L_Index - 1).Coeff_Gradients) *
                        dEdY;
      Weights_Error : constant Real_Float_Matrix :=
                        dEdY * This_Layer.Input_Data;
   begin
      Put_Line (Routine_Name & "layer " & Integer'Image (L_Index));
      Print_Float_Vector (Routine_Name & "dEdY ", dEdY, 1, 7);
      Print_Float_Vector (Routine_Name & "Input_Error ", Input_Error, 1, 7);

      This_Layer.Delta_Weights := This_Layer.Delta_Weights + Weights_Error;
      --        Print_Float_Matrix (Routine_Name & "Delta_Weights",
      --                            This_Layer.Delta_Weights, 1, 2, 1, 7);

      Print_Float_Vector (Routine_Name & "Delta_Bias ",
                          This_Layer.Delta_Bias, 1, 7);
      Print_Float_Vector (Routine_Name & "Output_Error ",
                          This_Layer.Output_Error, 1, 7);

      This_Layer.Delta_Bias := This_Layer.Delta_Bias + This_Layer.Output_Error;
      This_Layer.Passes := This_Layer.Passes + 1;
      Print_Float_Vector (Routine_Name & "updated Delta_Bias ",
                          This_Layer.Delta_Bias, 1, 7);
      Next_Layer.Output_Error := Input_Error;
      aModel.Layers (L_Index) := This_Layer;

   end Backward;

   --  -------------------------------------------------------------------------

   procedure Compile
     (aModel     : in out Sequential_Model; Num_Epochs : Positive;
      Learn_Rate : Float)
   is
      use Ada.Assertions;
      use Real_Float_Arrays;
      Routine_Name : constant String := "Neural_Model.Compile ";
      --  Loss_Deriv is dE/dY for output layer
      Loss         : Float           := 0.0;
      Optimiser    : Optimizer_Record (Optimizer_Adam);
      Params       : Parameters_List;  --  list of Parameters_Record
   begin
      C_Init (Optimiser.Adam, aModel.Connections);

      for epoch in 1 .. Num_Epochs loop
         Put_Line (Routine_Name & "epoch " & Integer'Image (epoch));
         for sample in 1 .. aModel.Num_Samples loop
            Put_Line (Routine_Name & "sample " & Integer'Image (sample));
            Forward (aModel, sample);
            --              Put_Line (Routine_Name & "Layers.Last_Element.Passes" &
            --                    Integer'Image (aModel.Layers.Last_Element.Passes));
            Back_Propogate (aModel, sample, Optimiser);

            for index in
              aModel.Connections.First_Index .. aModel.Connections.Last_Index
            loop
               Put_Line (Routine_Name & "Connections index " &
                           Integer'Image (index));
               Print_Float_Vector
                 (Routine_Name & "Intercept_Grads",
                  aModel.Connections (index).Intercept_Grads, 1, 7);
               --                 Put_Line (Routine_Name & "Connections index" &
               --                             Integer'Image (index));
               --                 Print_Matrix_Dimensions (Routine_Name & "Coeff_Gradients",
               --                                          aModel.Connections (index).Coeff_Gradients);
               --                 Print_Matrix_Dimensions (Routine_Name & "Delta_Weights",
               --                                          aModel.Layers (index + 1).Delta_Weights);
               --                 Put_Line (Routine_Name & "layer " & Integer'Image (index + 1) & " Passes" &
               --                             Integer'Image (aModel.Layers (index + 1).Passes));
               Assert
                 (aModel.Layers (index + 1).Passes > 0,
                  Routine_Name & "layer" & Integer'Image (index + 1) &
                    " passes is zero!");
               aModel.Connections (index).Coeff_Gradients :=
                 aModel.Connections (index).Coeff_Gradients -
                 Learn_Rate * aModel.Layers (index + 1).Delta_Weights /
                 Float (aModel.Layers (index + 1).Passes);
               aModel.Connections (index).Intercept_Grads :=
                 aModel.Connections (index).Intercept_Grads -
                 Learn_Rate * aModel.Layers (index + 1).Delta_Bias /
                 Float (aModel.Layers (index + 1).Passes);
               Print_Float_Vector (Routine_Name & "layer " &
                                     Integer'Image (index + 1) & " Delta_Bias",
                                   aModel.Layers (index + 1).Delta_Bias, 1, 7);
               Print_Float_Vector
                 (Routine_Name & "updated Intercept_Grads",
                  aModel.Connections (index).Intercept_Grads, 1, 7);
               --                 Print_Float_Vector
               --                   (Routine_Name & "layer " & Integer'Image (index + 1) &
               --                    " Delta_Bias",
               --                    Learn_Rate * aModel.Layers (index + 1).Delta_Bias /
               --                    Float (aModel.Layers (index + 1).Passes), 1, 6);
               --                 Print_Float_Matrix
               --                   (Routine_Name & "layer " & Integer'Image (index + 1) &
               --                    " Delta_Weights",
               --                    Learn_Rate * aModel.Layers (index + 1).Delta_Weights /
               --                    Float (aModel.Layers (index + 1).Passes),
               --                    1, 4, 1, 6);
               --                 New_Line;
            end loop;
            --              Put_Line (Routine_Name & "Connections updated");
         end loop;

         for c_index in
           aModel.Connections.First_Index .. aModel.Connections.Last_Index
         loop
            Put_Line (Routine_Name & "update c_index " & Integer'Image (c_index));
            Update
              (aModel.Connections (c_index), aModel.Layers (c_index + 1),
               Learn_Rate);
         end loop;
         Put_Line (Routine_Name & "epoch " & Integer'Image (epoch) & " done.");
         New_Line;
      end loop;

   end Compile;

   --  -------------------------------------------------------------------------

   function Deactivate
     (aModel : in out Sequential_Model; Sample, L_Index : Positive)
      return Real_Float_Vector
   is
      use Base_Neural;
      Routine_Name : constant String := "Neural_Model.Backward_Activation ";
      This_Layer   : constant Layer  := aModel.Layers (L_Index);
      Result       : Real_Float_Vector (This_Layer.Output_Error'Range);
   begin
      case This_Layer.Activation is
         when Identity_Activation =>
            Result := This_Layer.Output_Error;
         when Logistic_Activation =>
            Put_Line (Routine_Name & "Logistic_Activation not implemented");
         when ReLu_Activation =>
            Result := Rect_LU_Derivative (This_Layer.Output_Error);
         when Sigmoid_Activation =>
            Put_Line (Routine_Name & "Sigmoid_Activation not implemented");
         when Soft_Max_Activation =>
            Put_Line (Routine_Name & "Soft_Max_Activation not implemented");
      end case;
      Print_Float_Vector (Routine_Name & "Result", Result, 1, 7);

      return Result;

   end Deactivate;

   --  -------------------------------------------------------------------------

   function Deriv_ReLU (X : Real_Float_Vector) return Real_Float_Vector is
      RLU   : Real_Float_Vector := X;
      Deriv : Real_Float_Vector (X'Range);
   begin
      Base_Neural.Rect_LU (RLU);
      for index in X'Range loop
         if X (index) <= 0.0 then
            Deriv (index) := 0.0;
         else
            Deriv (index) := RLU (index) / X (index);
         end if;
      end loop;

      return RLU;

   end Deriv_ReLU;

   --  -------------------------------------------------------------------------
   --  When calculating the derivative of the softmax function,
   --  a Jacobian matrix which is the matrix of all first-order
   --  partial derivatives is needed.
   function Deriv_Softmax (X : Real_Float_Vector) return Real_Float_Matrix is
      --        Routine_Name : constant String := "Neural_Model.Deriv_Softmax ";
      Jacobian : Real_Float_Matrix (X'Range, X'Range);
   begin
      for row in Jacobian'Range loop
         for col in Jacobian'Range (2) loop
            if col = row then
               Jacobian (row, col) := X (col) * (1.0 - X (col));
            else
               Jacobian (row, col) := -X (col) * X (row);
            end if;
         end loop;
      end loop;

      return Jacobian;

   end Deriv_Softmax;

   --  -------------------------------------------------------------------------

   procedure Forward
     (aModel : in out Sequential_Model; Sample_Index : Positive)
   is
      use Real_Float_Arrays;
      use Base_Neural;
      use Stochastic_Optimizers;
      use Layer_Packge;
      Routine_Name : constant String := "Neural_Model.Forward ";
      Actual       : constant Real_Float_Vector :=
                       Get_Row (aModel.Labels, Sample_Index);
      Last_Layer   : Layer := aModel.Layers.Last_Element;
      Predicted    : Real_Float_Vector (aModel.Labels'Range (2));
      Loss         : Float := 0.0;
   begin
      aModel.Layers (1).Input_Data :=
        Get_Row (aModel.Input_Data, Sample_Index);

      for layer in aModel.Layers.First_Index + 1 .. aModel.Layers.Last_Index
      loop
         Put_Line (Routine_Name & "layer" & Integer'Image (layer));
         declare
            Input_Vec     : constant Real_Float_Vector :=
                              aModel.Layers (layer - 1).Nodes;
            Connect       : constant Parameters_Record :=
                              aModel.Connections (layer - 1);
            Updated_Nodes : constant Real_Float_Vector :=
                              Connect.Coeff_Gradients * Input_Vec +
                                Connect.Intercept_Grads;
         begin
            aModel.Layers (layer).Input_Data := aModel.Layers (layer - 1).Nodes;
            --              Print_Float_Vector
            --                (Routine_Name & "layer" & Integer'Image (layer) &
            --                   " Input_Data", aModel.Layers (layer).Input_Data, 1, 7);
            --              Print_Float_Vector (Routine_Name & "Input_Vec", Input_Vec, 1, 7);
            --              Print_Float_Matrix (Routine_Name & "Connect.Coeff_Gradients",
            --                                  Connect.Coeff_Gradients, 1, 2, 1, 7);
            Print_Float_Vector (Routine_Name & "Connect.Intercept_Grads",
                                Connect.Intercept_Grads, 1, 7);
            aModel.Layers (layer).Input_Data := Input_Vec;
            aModel.Layers (layer).Nodes := Updated_Nodes;

            --              Print_Float_Vector
            --                (Routine_Name & "after processing, layer" &
            --                   Integer'Image (layer) & " Input_Data",
            --                 aModel.Layers (layer).Input_Data, 1, 7);
            --              Print_Float_Vector (Routine_Name & "nodes",
            --                                  aModel.Layers (layer).Nodes, 1, 7);

            case aModel.Layers (layer).Activation is
               when Identity_Activation =>
                  null;
               when Logistic_Activation =>
                  Put_Line
                    (Routine_Name & "Logistic_Activation not implemented");
               when ReLu_Activation =>
                  Rect_LU (aModel.Layers (layer).Nodes);
               when Sigmoid_Activation =>
                  Put_Line
                    (Routine_Name & "Sigmoid_Activation not implemented");
               when Soft_Max_Activation =>
                  Softmax (aModel.Layers (layer).Nodes);
            end case;
         end;  --  declare block
      end loop;  --  layers

      Predicted := aModel.Layers.Last_Element.Nodes;
      Last_Layer.Passes := Last_Layer.Passes + 1;

      Print_Float_Vector (Routine_Name & "Predicted", Predicted);
      Print_Float_Vector
        (Routine_Name & "Actual", Get_Row (aModel.Labels, Sample_Index));

      case aModel.Loss_Method is
         when Loss_Binary_Log =>
            Put_Line (Routine_Name & "Binary_Log_Loss method not implemented");
         when Loss_Log =>
            Put_Line (Routine_Name & "Log_Loss method not implemented");
         when Loss_Mean_Square_Error =>
            Loss := Loss +
              Base_Neural.Mean_Squared_Error
                (Predicted, Get_Row (aModel.Labels, Sample_Index));
            Last_Layer.Output_Error :=
              Base_Neural.MSE_Derivative (Predicted, Actual);
            aModel.Layers (aModel.Layers.Last_Index) := Last_Layer;
      end case;
      Print_Float_Vector (Routine_Name & "Last_Layer.Output_Error",
                          Last_Layer.Output_Error, 1, 7);
      Print_Float_Vector (Routine_Name & "actual Last_Layer.Output_Error",
                          aModel.Layers (aModel.Layers.Last_Index).Output_Error,
                          1, 7);


      Put_Line (Routine_Name & "Loss" & Float'Image (Loss));
      New_Line;

   end Forward;

   --  ---------------------------------------------------------------------------

   --     function Get_Output_Value (aModel : Sequential_Model) return Real_Float_Vector is
   --     begin
   --        return aModel.Layers.Last_Element.Output_Data;
   --
   --     end Get_Output_Value;

   --  -------------------------------------------------------------------------

   function To_Matrix (Data : Real_Float_Vector) return Real_Float_Matrix is
      Result : Real_Float_Matrix (1 .. 1, Data'Range);
   begin
      for col in Data'Range loop
         Result (1, col) := Data (col);
      end loop;

      return Result;

   end To_Matrix;

   --  ---------------------------------------------------------------------------

   procedure Update
     (Connection : in out Stochastic_Optimizers.Parameters_Record;
      aLayer     : in out Layer; Learn_Rate : Float)
   is
      use Real_Float_Arrays;
      Routine_Name : constant String := "Neural_Model.Update ";
   begin
      --        Print_Float_Matrix
      --          (Routine_Name & "Coeff_Gradients", Connection.Coeff_Gradients,
      --           1, 2, 1, 6);
      --        Print_Float_Matrix
      --          (Routine_Name & "Delta_Weights", aLayer.Delta_Weights,
      --           1, 2, 1, 6);
      Print_Float_Vector
        (Routine_Name & "Intercept_Grads", Connection.Intercept_Grads, 1, 7);
      Connection.Coeff_Gradients :=
        Connection.Coeff_Gradients + aLayer.Delta_Weights;
      --        Print_Float_Matrix
      --          (Routine_Name & "updated Coeff_Gradients", Connection.Coeff_Gradients,
      --           1, 2, 1, 7);
      --        Print_Float_Vector
      --          (Routine_Name & "Delta_Bias", aLayer.Delta_Bias, 1, 7);
      Connection.Intercept_Grads :=
        Connection.Intercept_Grads + aLayer.Delta_Bias;
      Print_Float_Vector (Routine_Name & "updated Intercept_Grads",
                          Connection.Intercept_Grads, 1, 7);

      for row in aLayer.Delta_Weights'Range loop
         for col in aLayer.Delta_Weights'Range (2) loop
            aLayer.Delta_Weights (row, col) := 0.0;
         end loop;
         aLayer.Delta_Bias (row) := 0.0;
      end loop;

      aLayer.Passes := 0;

   end Update;

   --    -------------------------------------------------------------------------

end Neural_Model;
