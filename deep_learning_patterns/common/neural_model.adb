--  with Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Base_Neural;
with Basic_Printing; use Basic_Printing;
with Stochastic_Optimizers; use Stochastic_Optimizers;

package body Neural_Model is

   procedure Backward
     (aModel : in out Sequential_Model; L_Index : Positive);
   function Deactivate
     (aModel : in out Sequential_Model; L_Index : Positive)
      return Real_Float_Vector;
--     function Deriv_ReLU (X : Real_Float_Vector) return Real_Float_Vector;
--     function Deriv_Softmax (X : Real_Float_Vector) return Real_Float_Matrix;
   procedure Forward
     (aModel : in out Sequential_Model; Sample_Index : Positive);
   procedure Update
     (Connection : in out Stochastic_Optimizers.Parameters_Record;
      aLayer     : in out Layer; Learn_Rate : Float);

   --  -------------------------------------------------------------------------

   procedure Add_Connections (aModel : in out Sequential_Model) is
--        Routine_Name : constant String := "Neural_Model.Add_Connections ";
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

   procedure Add_Data (aModel           : in out Sequential_Model;
                       Features, Labels : Real_Float_Matrix) is
   begin
      aModel.Input_Data := Features;
      aModel.Labels := Labels;

   end Add_Data;

   --  ---------------------------------------------------------------------------
   --  Initialization - add first layer
   procedure Add_First_Layer (aModel : in out Sequential_Model)
   is
      First_Layer : Layer (aModel.Num_Features, aModel.Num_Features);
   begin
      aModel.Layers.Append (First_Layer);

   end Add_First_Layer;

   --  ---------------------------------------------------------------------------
   --  Initialization - add other layers
   procedure Add_Layer
     (aModel     : in out Sequential_Model; Num_Nodes : Positive;
      Activation :        Activation_Kind := Identity_Activation)
   is
--        Routine_Name : constant String := "Neural_Model.Add_Layer others  ";
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
     (aModel    : in out Sequential_Model) is
--        Optimiser : Stochastic_Optimizers.Optimizer_Record) is
--        Routine_Name : constant String := "Neural_Model.Back_Propogate ";
   begin
      for layer_id in reverse
        aModel.Layers.First_Index + 1 .. aModel.Layers.Last_Index loop
         Backward (aModel, layer_id);
      end loop;

   end Back_Propogate;

   --  -------------------------------------------------------------------------

   procedure Backward
     (aModel : in out Sequential_Model; L_Index : Positive) is
      use Real_Float_Arrays;
      Routine_Name  : constant String := "Neural_Model.Backward ";
      This_Layer    : Layer := aModel.Layers (L_Index);
      dEdY          : constant Real_Float_Vector :=
                        Deactivate (aModel, L_Index);
      --  Transpose of Coeff_Gradients is dX/dY
      Input_Error   : constant Real_Float_Vector :=
                        Transpose
                          (aModel.Connections (L_Index - 1).Coeff_Gradients) *
                        dEdY;
      Weights_Error : constant Real_Float_Matrix :=
                        dEdY * This_Layer.Input_Data;
   begin
      This_Layer.Delta_Weights := This_Layer.Delta_Weights + Weights_Error;
      This_Layer.Delta_Bias := This_Layer.Delta_Bias + This_Layer.Output_Error;
      This_Layer.Passes := This_Layer.Passes + 1;
      aModel.Layers (L_Index) := This_Layer;
      aModel.Layers (L_Index - 1).Output_Error := Input_Error;
      Print_Float_Vector (Routine_Name & "layer" & Integer'Image (L_Index) &
                            " Input_Error:", Input_Error);

   end Backward;

   --  -------------------------------------------------------------------------

   procedure Compile
     (aModel     : in out Sequential_Model; Num_Epochs : Positive;
      Learn_Rate : Float) is
      Routine_Name : constant String := "Neural_Model.Compile ";
      --  Loss_Deriv is dE/dY for output layer
--        Optimiser    : Optimizer_Record (Optimizer_Adam);
--        Params       : Parameters_List;  --  list of Parameters_Record
   begin
--        C_Init (Optimiser.Adam, aModel.Connections);

      for epoch in 1 .. Num_Epochs loop
         Put_Line (Routine_Name & "epoch " & Integer'Image (epoch));
         for sample in 1 .. aModel.Num_Samples loop
            Put_Line (Routine_Name & "sample " & Integer'Image (sample));
            Forward (aModel, sample);
            Back_Propogate (aModel);
         end loop;  --  sample

         for c_index in
           aModel.Connections.First_Index .. aModel.Connections.Last_Index
         loop
            Update
              (aModel.Connections (c_index), aModel.Layers (c_index + 1),
               Learn_Rate);
         end loop;
      end loop;  --  epoch

   end Compile;

   --  -------------------------------------------------------------------------

   function Deactivate
     (aModel : in out Sequential_Model; L_Index : Positive)
      return Real_Float_Vector
   is
      use Base_Neural;
      Routine_Name : constant String := "Neural_Model.Deactivate ";
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
            Result := Sigmoid_Derivative (This_Layer.Output_Error);
         when Soft_Max_Activation =>
            Put_Line (Routine_Name & "Soft_Max_Activation not implemented");
      end case;

      return Result;

   end Deactivate;

   --  -------------------------------------------------------------------------

--     function Deriv_ReLU (X : Real_Float_Vector) return Real_Float_Vector is
--        RLU   : Real_Float_Vector := X;
--        Deriv : Real_Float_Vector (X'Range);
--     begin
--        Base_Neural.Rect_LU (RLU);
--        for index in X'Range loop
--           if X (index) <= 0.0 then
--              Deriv (index) := 0.0;
--           else
--              Deriv (index) := RLU (index) / X (index);
--           end if;
--        end loop;
--
--        return RLU;
--
--     end Deriv_ReLU;

   --  -------------------------------------------------------------------------
   --  When calculating the derivative of the softmax function,
   --  a Jacobian matrix which is the matrix of all first-order
   --  partial derivatives is needed.
--     function Deriv_Softmax (X : Real_Float_Vector) return Real_Float_Matrix is
--        --        Routine_Name : constant String := "Neural_Model.Deriv_Softmax ";
--        Jacobian : Real_Float_Matrix (X'Range, X'Range);
--     begin
--        for row in Jacobian'Range loop
--           for col in Jacobian'Range (2) loop
--              if col = row then
--                 Jacobian (row, col) := X (col) * (1.0 - X (col));
--              else
--                 Jacobian (row, col) := -X (col) * X (row);
--              end if;
--           end loop;
--        end loop;
--
--        return Jacobian;
--
--     end Deriv_Softmax;

   --  -------------------------------------------------------------------------

   procedure Forward
     (aModel : in out Sequential_Model; Sample_Index : Positive) is
      use Real_Float_Arrays;
      use Base_Neural;
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
      aModel.Layers (1).Nodes := aModel.Layers (1).Input_Data;

      for layer in aModel.Layers.First_Index + 1 .. aModel.Layers.Last_Index
      loop
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
            aModel.Layers (layer).Input_Data := Input_Vec;
            aModel.Layers (layer).Nodes := Updated_Nodes;

            case aModel.Layers (layer).Activation is
               when Identity_Activation =>
                  null;
               when Logistic_Activation =>
                  Put_Line
                    (Routine_Name & "Logistic_Activation not implemented");
               when ReLu_Activation =>
                  Rect_LU (aModel.Layers (layer).Nodes);
               when Sigmoid_Activation =>
                  Sigmoid (aModel.Layers (layer).Nodes);
               when Soft_Max_Activation =>
                  Softmax (aModel.Layers (layer).Nodes);
            end case;
         end;  --  declare block
      end loop;  --  layers

      Predicted := aModel.Layers.Last_Element.Nodes;
      Last_Layer.Passes := Last_Layer.Passes + 1;

      --        Print_Float_Vector (Routine_Name & "Predicted", Predicted);
      --        Print_Float_Vector
      --          (Routine_Name & "Actual", Get_Row (aModel.Labels, Sample_Index));

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

      Print_Float_Vector (Routine_Name & "Predicted", Predicted);
      Print_Float_Vector (Routine_Name & "Actual", Actual);
      Put_Line (Routine_Name & "Loss" & Float'Image (Loss));
      New_Line;

   end Forward;

   --  ---------------------------------------------------------------------------

   function Get_Prediction (aModel : Sequential_Model) return Real_Float_Matrix is
      begin
         return aModel.Pred;

    end Get_Prediction;

   --  -------------------------------------------------------------------------

   procedure Update
     (Connection : in out Stochastic_Optimizers.Parameters_Record;
      aLayer     : in out Layer; Learn_Rate : Float)
   is
      use Real_Float_Arrays;
--        Routine_Name : constant String := "Neural_Model.Update ";
   begin
      Connection.Coeff_Gradients :=
        Connection.Coeff_Gradients -
          Learn_Rate * aLayer.Delta_Weights / Float (aLayer.Passes);
      Connection.Intercept_Grads :=
        Connection.Intercept_Grads -
          Learn_Rate * aLayer.Delta_Bias / Float (aLayer.Passes);

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
