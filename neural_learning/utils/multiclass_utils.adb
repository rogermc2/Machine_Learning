--  Based on scikit-learn/sklearn/utils.multiclass.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
--  with Ada.Text_IO; use Ada.Text_IO;

with Encode_Utils;
--  with Printing;

package body Multiclass_Utils is

   type Unique_Label_Types is (Unique_Multiclass, Unique_Indicator, Non_Unique);

   --  -------------------------------------------------------------------------

--     generic
--        type Item is private;
--        type Array_Type is array (Integer range <>) of Item;
--     function Check_Classification (Y : Array_Type) return Boolean;
--
--     function Check_Classification (Y : Array_Type) return Boolean is
--        Target_Type : constant Y_Type := Type_Of_Target (Y);
--     begin
--        return Y'Length /= 0;
--        return Target_Type in
--          Y_Binary | Y_Multiclass | Y_Multiclass_Multioutput |
--        Y_Multilabel_Indicator | Y_Multilabel_Sequences;

--     end Check_Classification;

   --  -------------------------------------------------------------------------
   --  L180
--        function Check_Classification_Targets (Y : Binary_Array) return Boolean is
--             function Check is new Check_Classification
--               (Item => Binary, Array_Type => Binary_Array);
--        begin
--           return Check (Y);
--
--        end Check_Classification_Targets;

   --  -------------------------------------------------------------------------
   --  L180
   function Check_Classification_Targets (Y : Binary_Matrix) return Boolean is
   begin
      return Type_Of_Target (Y) in
        Y_Binary | Y_Multiclass | Y_Multiclass_Multioutput |
      Y_Multilabel_Indicator | Y_Multilabel_Sequences;

   end Check_Classification_Targets;

   --  -------------------------------------------------------------------------
   --   --  L118
   function Is_Multilabel (Y : Integer_Array) return Boolean is
   begin
      pragma Unreferenced (Y);
      return False;

   end Is_Multilabel;

   --  -------------------------------------------------------------------------
   --  L118
   function Is_Multilabel (Y : NL_Types.Integer_List) return Boolean is
   begin
      pragma Unreferenced (Y);
      return False;

   end Is_Multilabel;

   --  -------------------------------------------------------------------------
   --  L118
   --  Examples:
   --  is_multilabel ([0, 1, 0, 1]) = False
   --  is_multilabel ([[1, 0], [0, 0]]) = True
   --  is_multilabel ([[1], [0], [0]]) = False
   --  is_multilabel ([[1, 0, 0]]) = True
   function Is_Multilabel (Y : Binary_Matrix) return Boolean is
   begin

      return Y'Length (2) > 1;

   end Is_Multilabel;

   --  -------------------------------------------------------------------------
   --  L118
   --  Examples:
   --  is_multilabel ([0, 1, 0, 1]) = False
   --  is_multilabel ([[1, 0], [0, 0]]) = True
   --  is_multilabel ([[1], [0], [0]]) = False
   --  is_multilabel ([[1, 0, 0]]) = True
   function Is_Multilabel (Y : Integer_Matrix) return Boolean is
      Multilabel : Boolean := False;
   begin
      if Y'Length (2) > 1 then
         Multilabel := Integer (Unique_Labels (Y).Length) < 3;
      end if;

      return Multilabel;

   end Is_Multilabel;

   --  -------------------------------------------------------------------------

   function Is_Multilabel (Y : NL_Types.Array_Of_Integer_Lists) return Boolean is
      use Ada.Containers;
      --          Routine_Name : constant String := "Multiclass_Utils.Is_Multilabel matrix ";
      Multilabel   : Boolean := False;
   begin
      for row in Y'Range loop
         if Y (row).Length > 1 then
            Multilabel := True;
         end if;
      end loop;

      if Multilabel then
         Multilabel := Integer (Unique_Labels (Y).Length) < 3;
      end if;

      return Multilabel;

   end Is_Multilabel;

   --  -------------------------------------------------------------------------
   --  L118
   function Is_Multilabel (Y : Real_Float_Matrix) return Boolean is
      use Ada.Containers;
      Labels     : NL_Types.Float_List;
      Multilabel : Boolean := Y'Length (2) > 1;
   begin
      if Multilabel then
         Labels := Unique_Labels (Y);
         Multilabel := Labels.Length < 3;
      end if;

      return Multilabel;

   end Is_Multilabel;

   --  -------------------------------------------------------------------------
   --   target_types:
   --   * binary: Y contains <= 2 discrete values and is 1d or a column
   --       vector.
   --   * continuous: Y is an array of floats that are not all integers and is 1d
   --       or a column vector.
   --   * continuous-multioutput: Y is a 2d array of floats that are not all
   --       integers and both dimensions are of size > 1.
   --   * multiclass: Y contains more than two discrete values, is not a
   --        sequence of sequences, and is 1d or a column vector.
   --   * multiclass-multioutput: Y is a 2d array that contains more
   --        than two discrete values, is not a sequence of sequences, and both
   --        dimensions are of size > 1.
   --   * multilabel-indicator: Y is a label indicator matrix, an array
   --        of two dimensions with at least two columns and at most two unique
   --        values.
   --   * unknown: Y is array-like but none of the above, such as a 3d array,
   --        sequence of sequences or an array of non-sequence objects.

   --     generic
   --        type Item is private;
   --        type Array_Type is array (Integer range <>) of Item;
   --     function Type_Of_Array_Target (Y : Array_Type) return Y_Type;
   --
   --     function Type_Of_Array_Target (Y : Array_Type) return Y_Type is
   --     begin
   --        pragma Unreferenced (Y);
   --        return Y_Binary;
   --
   --     end Type_Of_Array_Target;

   --  -------------------------------------------------------------------------

   --     function Type_Of_Target (Y : Binary_Array) is new Type_Of_Array_Target (Binary);
   function Type_Of_Target (Y : Binary_Array) return Y_Type is
   begin
      pragma Unreferenced (Y);
      return Y_Binary;

   end Type_Of_Target;

   --  -------------------------------------------------------------------------

   function Type_Of_Target (Y : Binary_Matrix ) return Y_Type is
      --       Routine_Name : constant String :=
      --                         "Multiclass_Utils.Type_Of_Target Binary_Matrix ";
      Result : Y_Type;
   begin
      if Is_Multilabel (Y) then
         Result := Y_Multilabel_Indicator;
      elsif Y'Length (2) > 1 then
         Result := Y_Multiclass_Multioutput;
      elsif Y'Length > 1 then
         Result := Y_Multiclass;
      else
         Result := Y_Binary;
      end if;

      return Result;

   end Type_Of_Target;

   --  -------------------------------------------------------------------------
   --  L202
   function Type_Of_Target (Y : Boolean_Matrix) return Y_Type is
      pragma Unreferenced (Y);
   begin
      return Y_Multilabel_Indicator;
   end Type_Of_Target;

   --  -------------------------------------------------------------------------
   --  L202
   function Type_Of_Target (Y : Integer_Array) return Y_Type is
      use Ada.Containers;
      Classes : constant NL_Types.Integer_List := Unique_Labels (Y);
      Result  : Y_Type;
   begin
      if Is_Multilabel (Y) then
         Result := Y_Multilabel_Indicator;
      elsif Classes.Length > 2 then
         Result := Y_Multiclass;
      else
         Result := Y_Binary;
      end if;

      return Result;

   end Type_Of_Target;

   --  -------------------------------------------------------------------------

   function Type_Of_Target (Y : NL_Types.Integer_List) return Y_Type is
      use Ada.Containers;
      Classes : constant NL_Types.Integer_List := Unique_Labels (Y);
      Result  : Y_Type;
   begin
      if Is_Multilabel (Y) then
         Result := Y_Multilabel_Indicator;
      elsif Classes.Length > 2 then
         Result := Y_Multiclass;
      else
         Result := Y_Binary;
      end if;

      return Result;

   end Type_Of_Target;

   --  -------------------------------------------------------------------------

   function Type_Of_Target (Y : NL_Types.Array_Of_Integer_Lists) return Y_Type is
      use Ada.Containers;
      Classes : constant NL_Types.Integer_List := Unique_Labels (Y);
      Result  : Y_Type;
   begin
      if Is_Multilabel (Y) then
         Result := Y_Multilabel_Indicator;
      elsif Classes.Length > 2 then
         Result := Y_Multiclass;
      else
         Result := Y_Binary;
      end if;

      return Result;

   end Type_Of_Target;

   --  -------------------------------------------------------------------------
   --  L202
   function Type_Of_Target (Y : Integer_Matrix) return Y_Type is
--        Routine_Name : constant String :=
--                                 "Multiclass_Utils.Type_Of_Target Integer_Matrix ";
      Result : Y_Type;
   begin
      if Y'Length (2) = 1 and Integer (Unique_Labels (Y).Length) < 3 then
         Result := Y_Binary;
      elsif Is_Multilabel (Y) then
         Result := Y_Multilabel_Indicator;
      elsif Y'Length (2) > 1 then
         Result := Y_Multiclass_Multioutput;
      elsif Y'Length > 1 then
         Result := Y_Multiclass;
      else
         Result := Y_Binary;
      end if;

      return Result;

   end Type_Of_Target;

   --  -------------------------------------------------------------------------
   --  L202
   function Type_Of_Target (Y : String_Array) return Y_Type is
   begin
      pragma Unreferenced (Y);
      return Y_Multiclass;

   end Type_Of_Target;

   --  -------------------------------------------------------------------------
   --  L202
   function Type_Of_Target (Y : String_Matrix) return Y_Type is
   begin
      pragma Unreferenced (Y);
      return Y_Multiclass_Multioutput;

   end Type_Of_Target;

   --  -------------------------------------------------------------------------
   --  L202
   function Type_Of_Target (Y : Real_Float_Vector) return Y_Type is
   begin
      pragma Unreferenced (Y);
      return Y_Continuous;

   end Type_Of_Target;

   --  -------------------------------------------------------------------------
   --  L202
   function Type_Of_Target (Y : Real_Float_Matrix) return Y_Type is
      Result : Y_Type;
   begin
      if Is_Multilabel (Y) then
         Result := Y_Multilabel_Indicator;
      elsif Y'Length > 1 and Y'Length (2) > 1 then
         Result := Y_Continuous_Multioutput;
      else
         Result := Y_Continuous;
      end if;

      return Result;

   end Type_Of_Target;

   --  -------------------------------------------------------------------------

   function Type_Of_Target (Y : Unbounded_String_Array) return Y_Type is
      use Ada.Containers;
      Classes : constant NL_Types.Unbounded_List := Unique_Labels (Y);
   begin
      if Classes.Length > 2 then
         return Y_Multiclass;
      else
         return Y_Binary;
      end if;

   end Type_Of_Target;

   --  -------------------------------------------------------------------------

   function Type_Of_Target (Y : Unbounded_String_Matrix) return Y_Type is
   begin
      pragma Unreferenced (Y);
      return Y_Multiclass;

   end Type_Of_Target;

   --  -------------------------------------------------------------------------
  --  unique_labels L101
   --     function Unique_Multiclass_Set (Y : Integer_Matrix)
   --                                     return Encode_Utils.Int_Sets.Set is
   --     begin
   --        --  L22 _unique_multiclass, return np.unique
   --        return Encode_Utils.Unique (Y);
   --
   --     end Unique_Multiclass_Set;

   --  -------------------------------------------------------------------------
   --  L42 unique_labels
   function Unique_Labels (Y : Binary_Matrix) return NL_Types.Integer_List is
      Routine_Name : constant String :=
                       "Multiclass_Utils.Unique_Labels Binary ";
      Label_Kind   : Y_Type := Type_Of_Target (Y);
      Uniques      : Unique_Label_Types := Non_Unique;
      Unique_Val   : Natural := 0;
      Result       : NL_Types.Integer_List;
   begin
      --  L78
      if Label_Kind = Y_Binary then
         Label_Kind := Y_Multiclass;
      end if;

      case Label_Kind is
         when Y_Binary | Y_Multiclass => Uniques := Unique_Multiclass;
         when Y_Multilabel_Indicator => Uniques := Unique_Indicator;
         when Y_Unknown | Y_Continuous | Y_Continuous_Multioutput |
              Y_Multiclass_Multioutput | Y_Multilabel_Sequences => null;
      end case;

      Assert (Uniques /= Non_Unique, Routine_Name &
                "Invalid label type: " & Y_Type'Image (Label_Kind));
      --  L111
      if Uniques = Unique_Indicator then
         for index in Y'Range (2) loop
            Result.Append (Unique_Val);
            Unique_Val := Unique_Val + 1;
         end loop;
      else  -- Uniques = Unique_Multiclass
         Assert (False, Routine_Name & "Unique_Multiclass not coded ");
      end if;

      return Result;

   end Unique_Labels;

   --  -------------------------------------------------------------------------
   --  L42 unique_labels
   function Unique_Labels (Y : Real_Float_Matrix) return NL_Types.Float_List is
   begin
      --  L101
      return Encode_Utils.Unique (Y);

   end Unique_Labels;

   --  -------------------------------------------------------------------------
   --  L42 unique_labels
   function Unique_Labels (Y : Integer_Array) return NL_Types.Integer_List is
   begin
      return Encode_Utils.Unique (Y);

   end Unique_Labels;

   --  -------------------------------------------------------------------------
  --  L42 unique_labels
   function Unique_Labels (Y : Integer_Array_List)
                           return NL_Types.Integer_List is
   begin
      return Encode_Utils.Unique (Y);

   end Unique_Labels;

   --  -------------------------------------------------------------------------
   --  L42 unique_labels
   function Unique_Labels (Y : Unbounded_String_Array_List)
                           return NL_Types.Unbounded_List is
   begin
      return Encode_Utils.Unique (Y);

   end Unique_Labels;

   --  -------------------------------------------------------------------------
   --  L42 unique_labels
   function Unique_Labels (Y : NL_Types.Integer_List)
                              return NL_Types.Integer_List is
   begin
      return Encode_Utils.Unique (Y);

   end Unique_Labels;

   --  -------------------------------------------------------------------------
   --  L42 unique_labels
   function Unique_Labels (Y : Integer_Matrix) return NL_Types.Integer_List is
      --        --  L84
      --        Label_Kind   : Label_Type := Unique_Binary;
      --        Label_Set    : Encode_Utils.Int_Sets.Set;
   begin
      --  L101
      --        case Label_Kind is
      --           --  L35
      --           when Unique_Binary | Unique_Multiclass =>
      --              --  L22
      --              Label_Set := Unique_Multiclass_Set (Y);
      --              --  L29
      --           when Unique_Mutilabel_Indicator => Null;
      --              Labels := Unique_Indicator (Y);
      --        end case;
      --  L111
      return Encode_Utils.Unique (Y);

   end Unique_Labels;

   --  -------------------------------------------------------------------------
   --  L42 unique_labels
   function Unique_Labels (Y : NL_Types.Array_Of_Integer_Lists)
                              return NL_Types.Integer_List is
   begin
      return Encode_Utils.Unique (Y);

   end Unique_Labels;

   --  -------------------------------------------------------------------------
   --  L42 unique_labels
   function Unique_Labels (Y : Unbounded_String_Array)
                           return NL_Types.Unbounded_List is
   begin
      --  L111
      return Encode_Utils.Unique (Y);

   end Unique_Labels;

   --  -------------------------------------------------------------------------
   --  L42 unique_labels
   function Unique_Labels (Y : Unbounded_String_Matrix)
                           return NL_Types.Unbounded_List is
   begin
      --  L111
      return Encode_Utils.Unique (Y);

   end Unique_Labels;

   --  -------------------------------------------------------------------------
--  L101
   --     function Unique_Label_Set (Y : Integer_Matrix)
   --                                return Encode_Utils.Int_Sets.Set is
   --        Labels       : Encode_Utils.Int_Sets.Set;
   --        --  L84
   --        Label_Kind   : Label_Type := Unique_Binary;
   --     begin
   --        case Label_Kind is
   --           --  L35
   --           when Unique_Binary | Unique_Multiclass =>
   --              --  L22
   --              Labels := Unique_Multiclass_Set (Y);
   --              --  L29
   --           when Unique_Mutilabel_Indicator => Null;
   --  --              Labels := Unique_Indicator (Y);
   --        end case;
   --
   --        return Labels;
   --
   --     end Unique_Label_Set;

   --  -------------------------------------------------------------------------

end Multiclass_Utils;
