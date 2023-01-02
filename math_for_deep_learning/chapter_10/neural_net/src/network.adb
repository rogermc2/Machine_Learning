
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Basic_Printing; use Basic_Printing;
with Shuffler;

package body Network is

   procedure Generate_Minibatch (X_Train,Y_Train  : Real_Float_Matrix;
                                 X_Batch, Y_Batch : out Real_Float_Matrix);

   --  -------------------------------------------------------------------------

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
      --        Routine_Name : constant String := "Network.Fit ";

      X_Batch      : Real_Float_Matrix (1 .. Batch_Size, X_Train'Range (2));
      Y_Batch      : Real_Float_Matrix (1 .. Batch_Size, Y_Train'Range (2));
      Output_Data  : Real_Float_List;
      Error        : Float;
      Back_Error   : Real_Float_List;
   begin
      Put_Line ("Running" & Integer'Image (Minibatches) & " minibatches");
      for count in 1 .. Minibatches loop
         New_Line;
         Put_Line ("minibatch" & Integer'Image (count));
         if count mod 40 = 0 then
            Put ("*");
         end if;
         Error := 0.0;
         --  Select a random minibatch
         Generate_Minibatch (X_Train, Y_Train, X_Batch, Y_Batch);
         --           Print_Float_Matrix (Routine_Name & "X_Batch", X_Batch, 1, 5, 42, 56);
         --           Print_Float_Matrix (Routine_Name & "Y_Batch", Y_Batch, 1, 5);

         --  forward propagation
         for sample in X_Batch'Range loop
            Output_Data.Clear;
            for col in X_Batch'Range (2) loop
               Output_Data.Append (X_Batch (Sample, col));
            end loop;

            for layer in Network.Layers.First_Index ..
              Network.Layers.Last_Index loop
               --                 Put_Line (Routine_Name &
               --                             Layer_Type'Image (Network.Layers (layer).Layer_Kind)
               --                              & " layer:" & Integer'Image (layer));
               Output_Data := Forward (Network.Layers (layer), Output_Data);
            end loop;

            --  accumulate loss
            declare
               Output_Vector : constant Real_Float_Vector :=
                                 To_Real_Float_Vector (Output_Data);
               Y_Vector      : Real_Float_Vector (Y_Batch'Range (2));
            begin
               for col in Y_Batch'Range (2) loop
                  Y_Vector (col) := Y_Batch (Sample, col);
               end loop;
               Error := Error + Loss (Y_Vector, Output_Vector);
               --                 Put_Line (Routine_Name & "Error" & Float'Image (Error));

               --  backward propagation
               Back_Error :=
                 To_Real_Float_List (Loss_Deriv (Y_Vector, Output_Vector));
               --                 Print_Real_Float_List (Routine_Name & "Back_Error", Back_Error);
            end;  --  declare block

            for layer in reverse Network.Layers.First_Index ..
              Network.Layers.Last_Index loop
               Back_Error := Backward (Network.Layers (layer), Back_Error);
            end loop;
         end loop;  --  Sample

         --  update weights and biases
         for layer in Network.Layers.First_Index ..
           Network.Layers.Last_Index loop
            Step (Network.Layers (layer), Learning_Rate);
         end loop;

         --  report mean loss over minibatch
         if Network.Verbose and then count mod 1000 = 0 then
            New_Line;
            Put_Line ("Minibatch" & Integer'Image (count) &
                        " error: " & Float'Image (Error));
         end if;
      end loop;
      New_Line;

   end Fit;

   --  -------------------------------------------------------------------------

   procedure Generate_Minibatch (X_Train, Y_Train  : Real_Float_Matrix;
                                 X_Batch, Y_Batch  : out Real_Float_Matrix) is
      Indices : Integer_Array (X_Train'Range);
   begin
      for index in Indices'Range loop
         Indices (index) := index;
      end loop;
      --              Print_Integer_Array (Routine_Name & "Indices", Indices, 1, 50);
      Shuffler.Shuffle (Indices);
      --              Print_Integer_Array (Routine_Name & "shuffled Indices", Indices, 1, 50);

      for row in X_Batch'Range loop
         for col in X_Batch'Range (2) loop
            X_Batch (row, col) := X_Train (Indices (row), col);
         end loop;

         for col in Y_Batch'Range (2) loop
            Y_Batch (row, col) := Y_Train (Indices (row), col);
         end loop;
      end loop;

   end Generate_Minibatch;

   --  -------------------------------------------------------------------------

   function Predict
     (Network : in out Network_Data; Input_Data : Real_Float_Matrix)
      return Real_Float_List_2D is
      --        Routine_Name : constant String := "Network.Predict ";
      Output_Data : Real_Float_List;
      Predictions : Real_Float_List_2D;  --  Result
   begin
      --  For each sample
      for row in Input_Data'Range loop
         Output_Data.Clear;
         for col in Input_Data'Range (2) loop
            Output_Data.Append (Input_Data (row, col));
         end loop;

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
