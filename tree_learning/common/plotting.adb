
with Ada.Text_IO; use Ada.Text_IO;

with PLplot_Standard; use PLplot_Standard;

with Maths;

with Plot_Utilities;

package body Plotting is

    procedure Display_Image (Data : ML_Types.Value_Data_List) is
      use Maths.Float_Math_Functions;
      Routine_Name   : constant String := "Plotting.Display_Image";
      Num_Image_Rows : constant Positive := Positive (Sqrt (Float (Data.Length)));
      Image          : PLplot_Auxiliary.Real_Matrix
          (1 .. Num_Image_Rows, 1 .. Num_Image_Rows);
    begin
        Put_Line (Routine_Name & "Num_Image_Rows: " &
                    Integer'Image (Num_Image_Rows));
        Put_Line (Routine_Name & "Plotting");
        Image := Plot_Utilities.To_PL_Array (Data, Num_Image_Rows);
        Plotting.Plot (Image);
    end Display_Image;
  
   --  -------------------------------------------------------------------------

   procedure Set_Grey_Colourmap (Num_Colours : Integer) is
      R   : constant Real_Vector (0 .. 1) := (0.0, 1.0);
      G   : constant Real_Vector (0 .. 1) := (0.0, 1.0);
      B   : constant Real_Vector (0 .. 1) := (0.0, 1.0);
      Pos : constant Real_Vector (0 .. 1) := (0.0, 1.0);
   begin
      Set_Number_Of_Colors_In_Color_Map_1 (Num_Colours);
      Set_Color_Map_1_Piecewise (RGB, Pos, R, G, B, Alt_Hue_Path_None);
      
   end Set_Grey_Colourmap;
   
   --  -------------------------------------------------------------------------

   procedure Plot (Bitmap : Real_Matrix) is
      Num_Rows    : constant Positive := Positive (Bitmap'Length);
      Num_Cols    : constant Positive := Positive (Bitmap'Length (2));
      Num_Colours : constant Integer := 255;
   begin
      Initialize_PLplot;
      Set_Grey_Colourmap (Num_Colours);

      Set_Environment (1.0, Long_Float (Num_Cols), 1.0, Long_Float (Num_Rows),
                       Justified, Box);
      --     Write_Labels ("", " ", "");
      Draw_Image_Color_Map_1_Automatic
        (Bitmap, 1.0, Long_Float (Num_Cols), 1.0, Long_Float (Num_Rows),
         0.0, 0.0, 1.0, Long_Float (Num_Cols), 1.0, Long_Float (Num_Rows));

      End_PLplot;
   
   end Plot;
   
end Plotting;
