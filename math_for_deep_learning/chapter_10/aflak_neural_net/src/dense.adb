
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Basic_Printing; use Basic_Printing;

package body Dense is

   procedure Backward
     (Layer         : in out Layer_Data; Out_Gradient : in out Real_Float_List;
      Learning_Rate : Float) is
      use Maths.Float_Math_Functions;
      use Real_Float_Arrays;
      Routine_Name  : constant String := "Dense.Backward ";
      Data_Mat      : constant Real_Float_Matrix :=
                        Transpose (Real_Float_Matrix (Layer.Input_Data));
      Gradient_Mat  : constant Real_Float_Matrix :=
                        To_Real_Float_Matrix (Out_Gradient);
   begin
      New_Line;
      Put_Line (Routine_Name & "Layer Kind: " &
                  Layer_Type'Image (Layer.Layer_Kind));
      Print_Matrix_Dimensions (Routine_Name & "Data_Mat", Data_Mat);
      Print_Matrix_Dimensions (Routine_Name & "Gradient_Mat",
                               Real_Float_Matrix (Gradient_Mat));
      if Layer.Layer_Kind = Dense_Layer then
         declare
            Weights_Gradient :  constant Real_Float_Matrix :=
                                 Data_Mat * Gradient_Mat;
            Input_Gradient   : constant Real_Float_Matrix :=
                                 Gradient_Mat *
                                   (Real_Float_Matrix (Layer.Weights));
         begin
            Print_Matrix_Dimensions (Routine_Name & "Layer.Weights)",
                                     Real_Float_Matrix (Layer.Weights));
            Print_Matrix_Dimensions (Routine_Name & "Input_Gradient",
                                     Input_Gradient);
            Layer.Weights :=
              Layer_Matrix (Real_Float_Matrix (Layer.Weights) -
                                Learning_Rate * Transpose (Weights_Gradient));
            Print_Matrix_Dimensions (Routine_Name & "Layer.Bias",
                                     Real_Float_Matrix (Layer.Bias));
            Layer.Bias :=
              Layer_Matrix (Real_Float_Matrix (Layer.Bias) -
                                Learning_Rate * Transpose (Gradient_Mat));

            Out_Gradient.Clear;
            for col in Input_Gradient'Range loop
               Out_Gradient.Append (Input_Gradient (1, col));
            end loop;
         end;

      else  --  Activation layer

         for index in Out_Gradient.First_Index .. Out_Gradient.Last_Index loop
            Out_Gradient.Replace_Element
              (index, Out_Gradient (index) *
               (1.0 - Tanh (Out_Gradient (index) ** 2)));
         end loop;
      end if;

   end Backward;

   --  --------------------------------------------------------------

   procedure Forward
     (Layer : in out Layer_Data; Data : in out Real_Float_List) is
      use Maths.Float_Math_Functions;
      use Real_Float_Arrays;
      --        Routine_Name : constant String := "Dense.Forward ";
      In_Data      : constant Real_Float_Matrix :=
                       To_Real_Float_Matrix (Data);
   begin
      --        Put_Line (Routine_Name & "Layer Kind: " &
      --                    Layer_Type'Image (Layer.Layer_Kind));
      --        Print_Matrix_Dimensions (Routine_Name & "Layer.Input_Data",
      --                                 Real_Float_Matrix (Layer.Input_Data));
      --        Print_Matrix_Dimensions (Routine_Name & "In_Data", In_Data);
      Layer.Input_Data := Layer_Matrix (In_Data);
      if Layer.Layer_Kind = Dense_Layer then
         declare
            Out_Mat : constant Real_Float_Matrix :=
                        Real_Float_Matrix (Layer.Weights) * Transpose (In_Data)
                      + Real_Float_Matrix (Layer.Bias);
         begin
            Data.Clear;
            for row in Out_Mat'Range loop
               Data.Append (Out_Mat (row, 1));
            end loop;
         end;

      else  --  Tanh Layer
         for index in Data.First_Index .. Data.Last_Index loop
            Data.Replace_Element (index, Tanh (Data (index)));
         end loop;
      end if;
      --        Print_List_Dimensions (Routine_Name & "Data out", Data);

   end Forward;

   --  --------------------------------------------------------------

end Dense;
