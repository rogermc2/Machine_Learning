
with PLplot_Auxiliary; use PLplot_Auxiliary;

with IL_Types;

package Plotting is
   
   procedure Display_Image (Data : IL_Types.Value_Data_List);
   procedure Plot (Bitmap : Real_Matrix);
   
end Plotting;
