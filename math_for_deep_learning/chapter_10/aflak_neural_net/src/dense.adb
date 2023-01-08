
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Basic_Printing; use Basic_Printing;

package body Dense is

   procedure Backward
     (Layer         : in out Layer_Data; Gradient : in out Real_Float_List;
      Learning_Rate : Float) is
      use Maths.Float_Math_Functions;
      use Real_Float_Arrays;
      Routine_Name  : constant String := "Dense.Backward ";
      Data_Mat      : constant Real_Float_Matrix :=
                        Real_Float_Matrix (Layer.Input_Data);
      Gradient_Mat  : constant Real_Float_Matrix :=
                        To_Real_Float_Matrix (Gradient);
   begin
      Put_Line (Routine_Name & "Layer Kind: " &
                  Layer_Type'Image (Layer.Layer_Kind));
      Print_Matrix_Dimensions (Routine_Name & "Transpose (Data_Mat)",
                               Real_Float_Matrix (Transpose (Data_Mat)));
      Print_Matrix_Dimensions (Routine_Name & "Gradient_Mat",
                               Real_Float_Matrix (Gradient_Mat));
      if Layer.Layer_Kind = Dense_Layer then
         declare
            Weights_Gradient :  constant Real_Float_Matrix :=
                                 Gradient_Mat * Transpose (Data_Mat);

            Input_Gradient   : constant Real_Float_Matrix :=
                                 Transpose (Real_Float_Matrix (Layer.Weights)) *
                                 Gradient_Mat;
         begin
            Layer.Weights :=
              Layer_Matrix (Real_Float_Matrix (Layer.Weights) -
                                Learning_Rate * Weights_Gradient);
            Layer.Bias :=
              Layer_Matrix (Real_Float_Matrix (Layer.Bias) -
                                Learning_Rate * Gradient_Mat);

            --              for index in Layer.Bias'Range (2) loop
            --                 Layer.Bias (1, index) :=
            --                   Layer.Bias (1, index) + Error (Integer (index));
            --              end loop;
            Gradient.Clear;
            for col in Input_Gradient'Range (2) loop
               Gradient.Append (Input_Gradient (1, col));
            end loop;
         end;

      else  --  Activation layer

         for index in Gradient.First_Index .. Gradient.Last_Index loop
            Gradient.Replace_Element
              (index, Gradient (index) * (1.0 - Tanh (Gradient (index) ** 2)));
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
