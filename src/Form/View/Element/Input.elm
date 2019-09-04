module Form.View.Element.Input exposing (..)


import Form.View2
import Form.Transaction exposing (Transaction)
import Form.FieldState as FieldState
import Form.Get as Get
import Form.Field as Field
import Element
import Element.Input
import Form.View.Element exposing (Element(..), Attribute, Label(..), Placeholder(..), LabelLocation(..), TextInputType(..))



text : (Field.Value String -> field) -> (FieldState.ErrorState error -> { attributes: List (Attribute error field msg), placeholder : Maybe (Placeholder error field msg), label: Label error field msg }) -> Element error field msg 
text field cont = 
    EL_TEXT TI_text (\errorState -> 
        let
            { attributes, placeholder, label } = cont errorState   
        in
        { attributes = attributes
        , placeholder = placeholder
        , label = label 
        , show = False
        , spellcheck = False
        })


spellChecked : (Field.Value String -> field) -> (FieldState.ErrorState error -> { attributes : List (Attribute error field msg), placeholder : Maybe (Placeholder error field msg), label: Label error field msg }) -> Element error field msg 
spellChecked field cont = 
    EL_TEXT TI_spellChecked (\errorState ->
        let 
            { attributes, placeholder, label } = cont errorState
        in 
        { attributes = attributes
        , placeholder = placeholder
        , label = label 
        , show = False
        , spellcheck = False
        })

search : (Field.Value String -> field) -> (FieldState.ErrorState error -> { attributes : List (Attribute error field msg), placeholder : Maybe (Placeholder error field msg), label: Label error field msg }) -> Element error field msg
search field cont = 
    EL_TEXT TI_search (\errorState ->
        let 
            { attributes, placeholder, label } = cont errorState
        in 
        { attributes = attributes
        , placeholder = placeholder
        , label = label 
        , show = False
        , spellcheck = False
        })

newPassword : (Field.Value String -> field) -> (FieldState.ErrorState error -> { attributes : List (Attribute error field msg), placeholder : Maybe (Placeholder error field msg), label: Label error field msg, show: Bool }) -> Element error field msg
newPassword field cont = 
    EL_TEXT TI_newPassword (\errorState ->
        let 
            { attributes, placeholder, label, show } = cont errorState
        in 
        { attributes = attributes
        , placeholder = placeholder
        , label = label 
        , show = show
        , spellcheck = False
        })

currentPassword : (Field.Value String -> field) -> (FieldState.ErrorState error -> { attributes : List (Attribute error field msg), placeholder : Maybe (Placeholder error field msg), label: Label error field msg, show: Bool }) -> Element error field msg
currentPassword field cont = 
    EL_TEXT TI_currentPassword (\errorState ->
        let 
            { attributes, placeholder, label, show } = cont errorState
        in 
        { attributes = attributes
        , placeholder = placeholder
        , label = label 
        , show = show
        , spellcheck = False
        })

username : (Field.Value String -> field) -> (FieldState.ErrorState error -> { attributes : List (Attribute error field msg), placeholder : Maybe (Placeholder error field msg), label: Label error field msg }) -> Element error field msg
username field cont = 
    EL_TEXT TI_username (\errorState ->
        let 
            { attributes, placeholder, label } = cont errorState
        in 
        { attributes = attributes
        , placeholder = placeholder
        , label = label 
        , show = False
        , spellcheck = False
        })

email : (Field.Value String -> field) -> (FieldState.ErrorState error -> { attributes : List (Attribute error field msg), placeholder : Maybe (Placeholder error field msg), label: Label error field msg }) -> Element error field msg
email field cont = 
    EL_TEXT TI_email (\errorState ->
        let 
            { attributes, placeholder, label } = cont errorState
        in 
        { attributes = attributes
        , placeholder = placeholder
        , label = label 
        , show = False
        , spellcheck = False
        })

multiline : (Field.Value String -> field) -> (FieldState.ErrorState error -> { attributes : List (Attribute error field msg), placeholder : Maybe (Placeholder error field msg), label: Label error field msg, spellcheck: Bool }) -> Element error field msg
multiline field cont = 
    EL_TEXT TI_multiline (\errorState ->
        let 
            { attributes, placeholder, label, spellcheck } = cont errorState
        in 
        { attributes = attributes
        , placeholder = placeholder
        , label = label 
        , show = False
        , spellcheck = spellcheck
        })
     

labelRight : List (Attribute error field msg) -> Element error field msg -> Label error field msg
labelRight =
    L_Label LL_labelRight

labelLeft : List (Attribute error field msg) -> Element error field msg -> Label error field msg
labelLeft =
    L_Label LL_labelLeft

labelAbove : List (Attribute error field msg) -> Element error field msg -> Label error field msg
labelAbove =
    L_Label LL_labelAbove

labelBelow : List (Attribute error field msg) -> Element error field msg -> Label error field msg
labelBelow =
    L_Label LL_labelBelow

labelHidden : String -> Label error field msg 
labelHidden =
    L_Hidden
