module Form.View.Element exposing (..)

import Form.View2 as InternalView exposing (Form)
import Form.Transaction exposing (Transaction)
import Form.FieldState as FieldState
import Form.Get as Get
import Form.Field as Field
import Element
import Element.Input

type ElementSingle 
    = ES_el
    | ES_LINK Link {url: String}

type Link
    = L_link
    | L_newTabLink
    | L_download
    | L_downloadAs {filename: String}

type ElementMultiple
    = EM_row 
    | EM_wrappedRow 
    | EM_column
    | EM_paragraph
    | EM_textColumn

type TextInputType 
    = TI_text
    | TI_spellChecked 
    | TI_search
    | TI_newPassword
    | TI_currentPassword
    | TI_username
    | TI_email
    | TI_multiline

type Element error field msg 
    = EL_INTERNAL (InternalView.View error field (Element.Element msg))
    | EL_SINGLE ElementSingle (List (Attribute error field msg)) (Element error field msg) 
    | EL_MULTIPLE ElementMultiple (List (Attribute error field msg)) (List (Element error field msg)) 
    | EL_TEXT TextInputType 
        (Field.Value String -> field) 
        (FieldState.ErrorState error -> 
            { attributes: List (Attribute error field msg)
            , placeholder : Maybe (Placeholder error field msg)
            , label : Label error field msg
            , show: Bool
            , spellcheck: Bool
            , onChange: String -> msg }
        )
    
type NearbyElement
    = NE_below
    | NE_above
    | NE_onRight
    | NE_onLeft
    | NE_inFront
    | NE_behindContent

type Attribute error field msg
    = AT_PURE (Element.Attribute msg)
    | AT_ELEMENT NearbyElement (Element error field msg)

type Placeholder error field msg 
    = PL_placeholder (List (Attribute error field msg)) (Element error field msg) 

type LabelLocation
    = LL_labelRight
    | LL_labelLeft
    | LL_labelAbove
    | LL_labelBelow

type Label error field msg
    = L_Label LabelLocation (List (Attribute error field msg)) (Element error field msg)
    | L_Hidden String

none : Element error field msg 
none = Element.none |> InternalView.VI_PURE |> EL_INTERNAL

text : String -> Element error field msg 
text s = Element.text s |> InternalView.VI_PURE |> EL_INTERNAL

el : List (Attribute error field msg) -> Element error field msg -> Element error field msg 
el = 
    EL_SINGLE ES_el

row : List (Attribute error field msg) -> List (Element error field msg) -> Element error field msg  
row =
    EL_MULTIPLE EM_row

wrappedRow : List (Attribute error field msg) -> List (Element error field msg) -> Element error field msg  
wrappedRow =
    EL_MULTIPLE EM_wrappedRow 

column : List (Attribute error field msg) -> List (Element error field msg) -> Element error field msg  
column =
    EL_MULTIPLE EM_column

-- SIZE 

type alias Length = Element.Length

width : Length -> Attribute error field msg
width length =
    AT_PURE (Element.width length)

height : Length -> Attribute error field msg
height length =
    AT_PURE (Element.height length)


below : Element error field msg -> Attribute error field msg 
below = AT_ELEMENT NE_below

above : Element error field msg -> Attribute error field msg 
above = AT_ELEMENT NE_above

onRight : Element error field msg -> Attribute error field msg 
onRight = AT_ELEMENT NE_onRight

onLeft : Element error field msg -> Attribute error field msg 
onLeft = AT_ELEMENT NE_onLeft

inFront : Element error field msg -> Attribute error field msg 
inFront = AT_ELEMENT NE_inFront

behindContent : Element error field msg -> Attribute error field msg 
behindContent = AT_ELEMENT NE_behindContent

centerX : Attribute error field msg 
centerX = AT_PURE Element.centerX 

centerY : Attribute error field msg 
centerY = AT_PURE Element.centerY 

alignTop : Attribute error field msg 
alignTop = AT_PURE Element.alignTop 

alignBottom : Attribute error field msg 
alignBottom = AT_PURE Element.alignBottom 

alignLeft : Attribute error field msg 
alignLeft = AT_PURE Element.alignLeft 

alignRight : Attribute error field msg 
alignRight = AT_PURE Element.alignRight 

spaceEvenly : Attribute error field msg 
spaceEvenly = AT_PURE Element.spaceEvenly 

scrollbars : Attribute error field msg 
scrollbars = AT_PURE Element.scrollbars 

scrollbarY : Attribute error field msg 
scrollbarY = AT_PURE Element.scrollbarY 

scrollbarX : Attribute error field msg 
scrollbarX = AT_PURE Element.scrollbarX 

clip : Attribute error field msg 
clip = AT_PURE Element.clip 

clipY : Attribute error field msg 
clipY = AT_PURE Element.clipY 

clipX : Attribute error field msg 
clipX = AT_PURE Element.clipX 

pointer : Attribute error field msg 
pointer = AT_PURE Element.pointer 

map : (msg1 -> msg2) -> Element error field msg1 -> Element error field msg2 
map f view = 
    case view of 
        EL_INTERNAL internal -> 
            EL_INTERNAL (InternalView.map (Element.map f) internal)

        EL_SINGLE es attributes element -> 
            EL_SINGLE es (List.map (mapAttribute f) attributes) (map f element)

        EL_MULTIPLE em attributes elements -> 
            EL_MULTIPLE em (List.map (mapAttribute f) attributes) (elements |> List.map (map f))

        EL_TEXT textInputType field cont -> 
            EL_TEXT 
                textInputType 
                field 
                (\error -> 
                    let
                        {attributes, placeholder, label, show, spellcheck, onChange} = cont error   
                    in 
                    { attributes = List.map (mapAttribute f) attributes
                    , placeholder = Maybe.map (mapPlaceholder f) placeholder
                    , label = mapLabel f label 
                    , show = show
                    , spellcheck = spellcheck
                    , onChange = onChange >> f
                    }
                )

mapPlaceholder : (msg1 -> msg2) -> Placeholder error field msg1 -> Placeholder error field msg2 
mapPlaceholder f placeholder = 
    case placeholder of 
        PL_placeholder attributes element -> 
            PL_placeholder (List.map (mapAttribute f) attributes) (map f element)

mapLabel : (msg1 -> msg2) -> Label error field msg1 -> Label error field msg2 
mapLabel f label = 
    case label of 
        L_Label labelLocation attributes element -> 
            L_Label labelLocation (List.map (mapAttribute f) attributes) (map f element)

        L_Hidden string -> 
            L_Hidden string 

mapAttribute : (msg1 -> msg2) -> Attribute error field msg1 -> Attribute error field msg2 
mapAttribute f attribute = 
    case attribute of 
        AT_PURE attribute_ -> 
            AT_PURE (Element.mapAttribute f attribute_)

        AT_ELEMENT ne element -> 
            AT_ELEMENT ne (map f element)

inForm : Form error field output -> Element error field msg -> Element.Element msg
inForm form element = 
    case element of 
        EL_INTERNAL internal -> 
            InternalView.inForm form internal

        EL_SINGLE elementSingle_ attributes element_ -> 
            elementSingleTransform elementSingle_ (List.map (inFormAttribute form) attributes) (inForm form element_)

        EL_MULTIPLE elementMultiple attributes elements -> 
            elementMultipleTransform elementMultiple  (List.map (inFormAttribute form) attributes) (List.map (inForm form) elements)

        EL_TEXT textInputType field cont -> 
            elementTextTransform textInputType field cont form 

inFormAttribute : Form error field output -> Attribute error field msg -> Element.Attribute msg 
inFormAttribute form attribute = 
    case attribute of 
        AT_PURE attribute_ -> 
            attribute_

        AT_ELEMENT nearbyElement element -> 
            case nearbyElement of 
                NE_below -> 
                    Element.below (inForm form element)

                NE_above -> 
                    Element.above (inForm form element)

                NE_onRight -> 
                    Element.onRight (inForm form element)

                NE_onLeft -> 
                    Element.onLeft (inForm form element)

                NE_inFront -> 
                    Element.inFront (inForm form element)

                NE_behindContent -> 
                    Element.behindContent (inForm form element)

inFormPlaceholder : Form error field output -> Placeholder error field msg -> Element.Input.Placeholder msg 
inFormPlaceholder form placeholder = 
    case placeholder of 
        PL_placeholder attributes element -> 
            Element.Input.placeholder (List.map (inFormAttribute form) attributes) (inForm form element)

inFormLabel : Form error field output -> Label error field msg -> Element.Input.Label msg 
inFormLabel form label = 
    case label of 
        L_Label labelLocation attributes element -> 
            let 
                labelFunction = 
                    case labelLocation of 
                        LL_labelRight -> 
                            Element.Input.labelRight

                        LL_labelLeft -> 
                            Element.Input.labelLeft

                        LL_labelAbove -> 
                            Element.Input.labelAbove

                        LL_labelBelow -> 
                            Element.Input.labelBelow

            in
            labelFunction (List.map (inFormAttribute form) attributes) (inForm form element)

        L_Hidden string -> 
            Element.Input.labelHidden string

elementSingleTransform : ElementSingle -> (List (Element.Attribute msg) -> Element.Element msg -> Element.Element msg)
elementSingleTransform es = 
    case es of 
        ES_el -> 
            Element.el 

        ES_LINK link_ {url} -> 
            case link_ of 
                L_link -> 
                    (\attributes label -> Element.link attributes { url = url, label = label})

                L_newTabLink -> 
                    (\attributes label -> Element.newTabLink attributes { url = url, label = label})

                L_download -> 
                    (\attributes label -> Element.download attributes { url = url, label = label})

                L_downloadAs {filename} -> 
                    (\attributes label -> Element.downloadAs attributes { url = url, label = label, filename = filename})

elementMultipleTransform: ElementMultiple -> (List (Element.Attribute msg) -> List (Element.Element msg) -> Element.Element msg)
elementMultipleTransform elementMultiple = 
    case elementMultiple of 
        EM_row -> 
            Element.row

        EM_wrappedRow -> 
            Element.wrappedRow

        EM_column -> 
            Element.column

        EM_paragraph -> 
            Element.paragraph

        EM_textColumn -> 
            Element.textColumn

elementTextTransform: 
    TextInputType 
    -> 
        (Field.Value String -> field) 
    -> 
        (FieldState.ErrorState error -> 
            { attributes: List (Attribute error field msg)
            , placeholder : Maybe (Placeholder error field msg)
            , label : Label error field msg 
            , show : Bool
            , spellcheck : Bool
            , onChange : String -> msg
            }
        ) 
    -> 
        Form error field output 
    -> 
        Element.Element msg 
elementTextTransform textInputType field cont form =
    let 
        getValue = Get.getString (Get.field field) form 
        value = getValue |> Get.toMaybe |> Maybe.withDefault ""
        error = Get.getError (Get.field field) form 
        {attributes, placeholder, label, show, spellcheck, onChange} = cont error
        attributes_ = List.map (inFormAttribute form) attributes
        placeholder_ = Maybe.map (inFormPlaceholder form) placeholder
        label_ = inFormLabel form label
    in 
    case textInputType of 
        TI_text -> 
            Element.Input.text attributes_ { placeholder = placeholder_, label = label_, onChange = onChange, text = value }

        TI_spellChecked  -> 
            Element.Input.spellChecked attributes_ { placeholder = placeholder_, label = label_, onChange = onChange, text = value }

        TI_search -> 
            Element.Input.search attributes_ { placeholder = placeholder_, label = label_, onChange = onChange, text = value }

        TI_newPassword -> 
            Element.Input.newPassword attributes_ { placeholder = placeholder_, label = label_, onChange = onChange, text = value, show = show }

        TI_currentPassword -> 
            Element.Input.currentPassword attributes_ { placeholder = placeholder_, label = label_, onChange = onChange, text = value, show = show }

        TI_username -> 
            Element.Input.username attributes_ { placeholder = placeholder_, label = label_, onChange = onChange, text = value }

        TI_email -> 
            Element.Input.email attributes_ { placeholder = placeholder_, label = label_, onChange = onChange, text = value }

        TI_multiline  -> 
            Element.Input.multiline attributes_ { placeholder = placeholder_, label = label_, onChange = onChange, text = value, spellcheck = spellcheck }

