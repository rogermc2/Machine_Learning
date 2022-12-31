
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Basic_Printing; use Basic_Printing;
with Shuffler;

package body Network is

   procedure Add_Activation_Layer (Network : in out Network_List) is
      Layer : Layer_Data (Activation_Layer, 0, 0);
   begin
      Network.Append (Layer);

   end Add_Activation_Layer;

   --  -------------------------------------------------------------------------

   procedure Add_Activation_Layer (Network    : in out Network_List;
                                   Input_Size : Layer_Range) is
      Layer : Layer_Data (Activation_Layer, Input_Size, 0);
   begin
      Network.Append (Layer);

   end Add_Activation_Layer;

   --  -------------------------------------------------------------------------

   procedure Add_Fully_Connected_Layer
     (Network : in out Network_List; Input_Size, Output_Size : Layer_Range) is
      Layer : Layer_Data (Hidden_Layer, Input_Size, Output_Size);
      use Maths;
   begin
      for row in 1 ..Input_Size loop
         for col in 1 .. Output_Size loop
            Layer.Weights (Row,Col) := 0.5 * Random_Float;
         end loop;
      end loop;

      for index in 1 .. Output_Size loop
         Layer.Bias (index) := 0.5 * Random_Float;
      end loop;

      Network.Append (Layer);

   end Add_Fully_Connected_Layer;

   --  -------------------------------------------------------------------------

   procedure Fit
     (Network       : in out Network_Data; X_Train : Real_Float_Matrix;
      Y_Train       : Real_Float_Matrix; Minibatches : Positive;
      Learning_Rate : Float; Batch_Size : Positive := 64) is
      use Real_Float_Arrays;
      Routine_Name : constant String := "Network.Fit ";

      procedure Do_Layer (Layer : in out Layer_Data;
                          Data  : in out Real_Float_List) is
      begin
         Data := Forward (Layer, Data);

      end Do_Layer;

      X_Batch      : Real_Float_Matrix (1 .. Batch_Size, X_Train'Range (2));
      Y_Batch      : Real_Float_Matrix (1 .. Batch_Size, Y_Train'Range (2));
      Output_Data  : Real_Float_List;
      Y_Vector     : Real_Float_Vector (Y_Batch'Range (2));
      Error        : Float;
      Back_Error   : Real_Float_List;
   begin
      for count in 1 .. Minibatches loop
--           Put_Line (Routine_Name & "Minibatch" & Integer'Image (count));
         Error := 0.0;
         --  Select a random minibatch
         declare
            Indices : Integer_Array (X_Train'Range);
         begin
            for index in Indices'Range loop
               Indices (index) := index;
            end loop;
            Shuffler.Shuffle (Indices);

            for row in X_Batch'Range loop
               for col in X_Batch'Range (2) loop
                  X_Batch (row, col) := X_Train (Indices (row), col);
               end loop;

               for col in Y_Batch'Range (2) loop
                  Y_Batch (row, col) := Y_Train (Indices (row), col);
               end loop;
            end loop;
         end;  --  declare block

         --  forward propagation
         for sample in X_Batch'Range loop
            Output_Data.Clear;
--              Put_Line (Routine_Name & "sample" & Integer'Image (sample));
            for col in X_Batch'Range (2) loop
               Output_Data.Append (X_Batch (Sample, col));
            end loop;

            for col in Y_Batch'Range (2) loop
               Y_Vector (col) := Y_Batch (Sample, col);
            end loop;

            for layer in Network.Layers.First_Index ..
              Network.Layers.Last_Index loop
               Do_Layer (Network.Layers (layer), Output_Data);
            end loop;

            declare
               Output_Vector : constant Real_Float_Vector := To_Real_Float_Vector (Output_Data);
            begin
               Error := Error + Loss (Y_Vector, Output_Vector);
--                 Put_Line (Routine_Name & "Error" & Float'Image (Error));

               --  backward propagation
               Back_Error :=
                 To_Real_Float_List (Loss_Deriv (Y_Vector, Output_Vector));
--                 Print_Real_Float_List (Routine_Name & "Back_Error", Back_Error);
            end;

            for layer in reverse Network.Layers.First_Index ..
              Network.Layers.Last_Index loop
--                 Put_Line (Routine_Name & "backward layer" &
--                             Integer'Image (layer));
               Back_Error := Backward (Network.Layers (layer), Back_Error);
            end loop;
         end loop;

         --  update weights and biases
         for layer in Network.Layers.First_Index ..
           Network.Layers.Last_Index loop
            Step (Network.Layers (layer), Learning_Rate);
         end loop;

         --  report mean loss over minibatch
         if Network.Verbose and then count mod 10 = 0 then
            Put_Line ("Minibatch" & Integer'Image (count) &
                        " error: " & Float'Image (Error));
         end if;
      end loop;

      Put_Line (Routine_Name & "done");

   end Fit;

   --  -------------------------------------------------------------------------

   function Predict
     (Network : in out Network_Data; Input_Data : Real_Float_Matrix)
      return Real_Float_List_2D is
      Routine_Name : constant String := "Network.Fit ";
      Output_Data : Real_Float_List;
      Predictions : Real_Float_List_2D;
   begin
      --  For each sample
      for row in Input_Data'Range loop
         for col in Input_Data'Range (2) loop
            Put_Line (Routine_Name & "row, col:" & Integer'Image (row) & "," &
                     Integer'Image (col));
            Output_Data (col) := Input_Data (row, col);
         end loop;
         Put_Line (Routine_Name & "Output_Data set");

         for layer in Network.Layers.First_Index ..
           Network.Layers.Last_Index loop
            Output_Data := Forward (Network.Layers (layer), Output_Data);
         end loop;
         Predictions.Append (Output_Data);
      end loop;

      return Predictions;

   end Predict;

   --  -------------------------------------------------------------------------

   procedure Print_Layer_Data (Name : String; Layer : Layer_Data) is
   begin
      Put_Line (Name);
      Put_Line ("Layer_Kind " & Layer_Type'Image (Layer.Layer_Kind));
      Put_Line ("Input_Size" & Layer_Range'Image (Layer.Input_Size));
      if Layer.Layer_Kind = Hidden_Layer then
         Print_Matrix_Dimensions ("Weights Size", Real_Float_Matrix
                                  (Layer.Weights));
         Put_Line ("Bias Size" & Layer_Range'Image (Layer.Bias'Length));
         Print_Matrix_Dimensions
           ("Delta_W Size", Real_Float_Matrix (Layer.Delta_W));
         Put_Line ("Bias Size" & Layer_Range'Image (Layer.Delta_B'Length));
      end if;

   end Print_Layer_Data;

   --  -------------------------------------------------------------------------

   procedure Print_Network_Data (Name : String; Network : Network_Data) is
   begin
      Put_Line (Name);
      for index in Network.Layers.First_Index ..
        Network.Layers.Last_Index loop
         Put_Line ("Layer" & Integer'Image (index));
         Put_Line ("Layer_Kind " & Layer_Type'Image
                   (Network.Layers.Element (index).Layer_Kind));
         Put_Line ("Input_Size" & Layer_Range'Image
                   (Network.Layers (index).Input_Size));
         if Network.Layers.Element (index).Layer_Kind = Hidden_Layer then
            Print_Matrix_Dimensions
              ("Weights Size", Real_Float_Matrix
                 (Network.Layers.Element (index).Weights));
            Put_Line ("Bias Size" & Layer_Range'Image
                      (Network.Layers (index).Bias'Length));
            Print_Matrix_Dimensions
              ("Delta_W Size", Real_Float_Matrix
                 (Network.Layers.Element (index).Delta_W));
            Put_Line ("Bias Size" & Layer_Range'Image
                      (Network.Layers.Element (index).Delta_B'Length));
         end if;
      end loop;

   end Print_Network_Data;

   --  -------------------------------------------------------------------------

end Network;
