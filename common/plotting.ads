
with PLplot_Auxiliary; use PLplot_Auxiliary;

with ML_Types;

package Plotting is
   
   procedure Display_Image (Data : ML_Types.Value_Data_List);
   procedure Plot (Bitmap : Real_Matrix);
   
end Plotting;
