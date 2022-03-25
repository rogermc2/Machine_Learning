
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--  with Build_Utils;
with Classifier_Types; use Classifier_Types;
with Criterion;
with Estimator;
with NL_Types; use NL_Types;
--  with Base_Decision_Tree;
with Encode_Utils;
with Export_Types;
--  with Node_Splitter;
--  with Tree;
with Weights;

package Printing is

   Print_Error : Exception;

   procedure Print_Boolean_Matrix (Name    : String;
                                   aMatrix : Estimator.Boolean_Matrix);
   procedure Print_Bounds (Name : String; Data : Export_Types.Bounds_List);
   procedure Print_Colours_List (Name    : String;
                                 Colours : Export_Types.Colours_List);
   procedure Print_Criterion (Name : String;
                              Data : Criterion.Criterion_Class);
   procedure Print_Integer_Colours_List
     (Name : String; Colours : Export_Types.Integer_Colours_List);
   procedure Print_Integer_Array (Name : String; anArray : Integer_Array);
   procedure Print_Export_Map (Name : String; aMap : Export_Types.Export_Map);
   procedure Print_Float_Array (Name   : String; anArray : Float_Array;
                                Start  : Integer := 1;
                                Finish : Integer := 0);
   procedure Print_Float_List (Name  : String; theList : Float_List);
   procedure Print_Indefinite_List (Name    : String;
                                    theList : Indef_String_List);
   procedure Print_Integer_List (Name  : String; theList : Integer_List;
                                 Start : Positive := 1; Last : Positive := 10);
   procedure Print_Integer_List (Name    : String;
                                 theList : Integer_DL_List);
   procedure Print_Integer_Set (Name   : String;
                                theSet : Encode_Utils.Int_Sets.Set);
   procedure Print_Natural_Lists_2D (Name : String;
                                     Data : Natural_Lists_2D);
   procedure Print_Float_Lists_2D (Name : String; Data : Float_List_2D);
   procedure Print_Float_Lists_3D (Name : String; Data : Float_List_3D);
   procedure Print_Multi_Value_Array (Name    : String;
                                      anArray : Multi_Value_Array);
   procedure Print_Natural_List (Name : String; theList : Natural_List);
   procedure Print_RGB_Array (Name : String; anArray : Export_Types.RGB_Array);
   procedure Print_Slice (Name : String; theSlice : Slice_Record);
   procedure Print_Slices (Name  : String; theList : Slices_List;
                           Start : Positive := 1; Last : Positive := 10);
   procedure Print_Strings (Name : String; theList : String_List);
   procedure Print_Strings (Name : String; theList : String_Vector);
   procedure Print_Strings (Name    : String;
                            theList : Indef_String_List);
   procedure Print_Unbounded_List (Name    : String;
                                   theList : Unbounded_List);
   procedure Print_Unbounded_Set (Name   : String;
                                  theSet : Encode_Utils.UB_String_Sets.Set);
   procedure Print_Value_Data_List (Name    : String;
                                    theList : Value_Data_List);
   procedure Print_Value_Data_Lists_2D (Name      : String;
                                        theList   : Value_Data_Lists_2D;
                                        Num_Items : Positive := 1000);
   procedure Print_Value_Data_Lists_3D (Name    : String;
                                        theList : Value_Data_Lists_3D);
   procedure Print_Value_Record (Name : String; Value : Value_Record);
   procedure Print_Weights (Name : String; Data : Weights.Weight_List);
   procedure Print_Weight_Lists_2D (Name : String;
                                    Data : Weights.Weight_Lists_2D);
   procedure Print_Weight_Lists_3D (Name : String;
                                    Data : Weights.Weight_Lists_3D);

end Printing;
