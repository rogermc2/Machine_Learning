
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Basic_Printing; use Basic_Printing;
with Losses;

package body Network is

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
            Layer.Weights (Row,Col) := Random_Float;
         end loop;
      end loop;

      for col in 1 .. Output_Size loop
         Layer.Bias (1, col) := Random_Float;
      end loop;

      Network.Append (Layer);

   end Add_Fully_Connected_Layer;

   --  -------------------------------------------------------------------------

   procedure Predict
     (Network : in out Network_Data; Data : Real_Float_List) is
      --        Routine_Name : constant String := "Network.Predict ";
      Output_Data : Real_Float_List := Data;
   begin
         for layer in Network.Layers.First_Index ..
           Network.Layers.Last_Index loop
            Forward (Network.Layers (layer), Output_Data);
         end loop;

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

   procedure Train
     (Network       : in out Network_Data; X_Train : Real_Float_Matrix;
      Y_Train       : Binary_Matrix; Epochs : Positive := 1000;
      Learning_Rate : Float := 0.01; Verbose : Boolean := True) is
      Routine_Name : constant String := "Network.Train ";
      Output_Data  : Real_Float_List;
      Grad         : Real_Float_Vector (Y_Train'Range (2));
      Error        : Float;
   begin
      for count in 1 .. Epochs loop
         if count mod 40 = 0 then
            Put ("*");
         end if;
         Error := 0.0;
         for sample in X_Train'Range loop
--              Put_Line ("sample" & Integer'Image (sample));
--  forward propagate
            Output_Data := To_Real_Float_List (Get_Row (X_Train, sample));
              Predict (Network, Output_Data);
            Error := Error + Losses.MSE (Get_Row (Y_Train, sample),
                                         Output_Data);
            Grad := Losses.MSE_Prime (Get_Row (Y_Train, sample), Output_Data);

            for layer in Network.Layers.First_Index ..
              Network.Layers.Last_Index loop
--                 Put_Line (Routine_Name &
--                             Layer_Type'Image (Network.Layers (layer).Layer_Kind)
--                           & " layer:" & Integer'Image (layer));
               Forward (Network.Layers (layer), Output_Data);
            end loop;
            --              Print_Float_Matrix (Routine_Name & "Y_Batch", Y_Batch, sample, sample);
            --              Print_Real_Float_List(Routine_Name & "Output_Data", Output_Data);

            for layer in reverse Network.Layers.First_Index ..
              Network.Layers.Last_Index loop
               Put_Line ("backward layer" & Integer'Image (layer));
               Backward (Network.Layers (layer), Error);
            end loop;
            --              Print_List_Dimensions (Routine_Name & "backward Error", Error);
            --              if sample = 1 then
            --                 Print_Real_Float_List (Routine_Name & "backward Error", Error);
            --              end if;

         end loop;  --  Sample
         --           Print_List_Dimensions (Routine_Name & "minibatch Loss", Loss);
         --           Print_Real_Float_List (Routine_Name & "minibatch Loss", Loss, 1, 6);

         --  report mean loss over minibatch
         if Network.Verbose and then
           (Epochs < 10 or else count mod (Epochs / 10) = 0) then
            New_Line;
            Put_Line ("Epoch" & Integer'Image (count) &
                        " mean error: " & Float'Image (Error));
         end if;
      end loop;  --  Minibatches
      New_Line;

   end Train;

   --  -------------------------------------------------------------------------

end Network;
