
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Types;

package Printing is

   Print_Error : Exception;

   procedure Print_Array_Of_Integer_Lists
     (Name    : String; theArray : ML_Types.Array_Of_Integer_Lists;
      Start   : Integer := 1; Finish : Integer := 0);
   procedure Print_Integer_List (Name    : String;
                                 theList : ML_Types.Integer_List;
                                 Start : Positive := 1; Last : Positive := 10);
   procedure Print_Integer_List (Name    : String;
                                 theList : ML_Types.Integer_DL_List);
   procedure Print_Strings (Name : String; theList : ML_Types.String_List);
   procedure Print_Strings (Name : String; theList : ML_Types.String_Vector);
   procedure Print_Strings (Name    : String;
                            theList : ML_Types.Indef_String_List);
   procedure Print_Unbounded_List (Name    : String;
                                   theList : ML_Types.Unbounded_List);
   procedure Print_Value_Data_List (Name    : String;
                                    theList : ML_Types.Value_Data_List);
   procedure Print_Value_Data_Lists_2D (Name      : String;
                                        theList   : ML_Types.Value_Data_Lists_2D;
                                        Num_Items : Positive := 1000);
   procedure Print_Value_Data_Lists_3D (Name    : String;
                                        theList : ML_Types.Value_Data_Lists_3D);
   procedure Print_Value_Record (Name : String; Value : ML_Types.Value_Record);

end Printing;
