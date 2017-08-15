{-# LANGUAGE MultiWayIf #-}
module Common where

import Data.List
import Debug.Trace (trace)
import qualified Utils as U
import qualified IDLSyn as I
import qualified Language.Haskell.Syntax as H (Module(..))
import Data.Char (isSpace, toUpper)
import SplitBounds (parts)
import Data.Map (Map)
import qualified Data.Map as M (fromList, lookup, toList, fromListWith, elems, mapWithKey)
import BasicTypes (Size(Short, Long, LongLong), ParamDir(..))
import Data.Maybe (fromMaybe)
import Data.Either (rights)
import Data.Monoid ((<>))
import Data.Tuple (swap)
import qualified Language.Haskell.Syntax as H
import qualified IDLUtils
import IDLUtils hiding (getDef)

getDef = jsname' . IDLUtils.getDef

jsname' "object" = "GObject"
jsname' "DOMApplicationCache" = "ApplicationCache"
jsname' "DOMWindowCSS" = "CSS"
jsname' "DOMCoreException" = "DOMException"
jsname' "DOMFormData" = "FormData"
jsname' "DOMURL" = "URL"
jsname' "DOMSecurityPolicy" = "SecurityPolicy"
jsname' "DOMSelection" = "Selection"
jsname' "DOMWindow" = "Window"
jsname' "DOMMimeType" = "MimeType"
jsname' "DOMMimeTypeArray" = "MimeTypeArray"
jsname' "DOMPlugin" = "Plugin"
jsname' "DOMPluginArray" = "PluginArray"
jsname' "DOMPath" = "Path2D"
jsname' "WebAnimation" = "Animation"
jsname' "DOMCSSNamespace" = "CSS"
jsname' "FetchBody" = "Body"
jsname' "FetchHeaders" = "Headers"
jsname' "FetchRequest" = "Request"
jsname' "FetchResponse" = "Response"
jsname' "DeprecatedCSSOMCounter" = "Counter"
jsname' "DeprecatedCSSOMPrimitiveValue" = "CSSPrimitiveValue"
jsname' "DeprecatedCSSOMRGBColor" = "RGBColor"
jsname' "DeprecatedCSSOMRect" = "Rect"
jsname' "DeprecatedCSSOMValue" = "CSSValue"
jsname' "DeprecatedCSSOMValueList" = "CSSValueList"
jsname' x = x

--jsname "CryptoKey" = "Key"
--jsname "CryptoKeyPair" = "KeyPair"
jsname "SubtleCrypto" = "WebKitSubtleCrypto"
jsname "DOMNamedFlowCollection" = "WebKitNamedFlowCollection"
jsname "MediaKeyError" = "WebKitMediaKeyError"
jsname "MediaKeyMessageEvent" = "WebKitMediaKeyMessageEvent"
jsname "MediaKeySession" = "WebKitMediaKeySession"
jsname "MediaKeys" = "WebKitMediaKeys"
jsname "MediaStream" = "webkitMediaStream"
jsname "RTCPeerConnection" = "webkitRTCPeerConnection"
--jsname "AudioContext" = "webkitAudioContext"
--jsname "OfflineAudioContext" = "webkitOfflineAudioContext"
jsname "PannerNode" = "webkitAudioPannerNode"
jsname "DataCue" = "WebKitDataCue"
jsname x = jsname' x

-- Helpers to produce class and datatype identifiers out of DOM identifiers

classFor s = "Is" ++ typeFor s
--typeFor  "Range" = "DOMRange"
--typeFor  "Screen" = "DOMScreen"
--typeFor  "Attr" = "DOMAttr"
typeFor  "Key" = "CryptoKey"
typeFor  "AlgorithmIdentifier" = "DOMString"
typeFor  "KeyFormat" = "DOMString"
-- typeFor  "XMLHttpRequestResponseType" = "DOMString"
typeFor  "custom" = "CanvasStyle"
typeFor "IceTransportState" = "RTCIceTransportState"
typeFor "IceGatheringState" = "RTCIceGatheringState"
typeFor "RtpTransceiverDirection" = "RTCRtpTransceiverDirection"
typeFor "TypedArray" = "RawTypedArray"
typeFor  s = jsname' s
fixType (I.TyName s x) = I.TyName (typeFor s) x
fixType (I.TyOptional a) = I.TyOptional (fixType a)
fixType (I.TyPromise a) = I.TyPromise (fixType a)
fixType x = x
fixDefn (I.Attribute a b c d e) = I.Attribute a b (fixType c) d e
fixDefn (I.Operation a b c d e) = I.Operation (fixId a) (fixType b) c d e
fixDefn (I.Interface a b c d e) = I.Interface a b (map fixDefn c) d e
fixDefn x = x
fixId (I.FunId a b c) = I.FunId (fixId a) b (map fixParam c)
fixId x = x
fixParam (I.Param a b c d e) = I.Param a b (fixType c) d e

inWebKitGtk = (`elem` ["Attr", "AudioTrack", "AudioTrackList", "BarProp", "BatteryManager", "Blob",
                    "CDATASection", "Comment", "CSSRule", "CSSRuleList", "CSSStyleDeclaration",
                    "CSSStyleSheet", "CSSValue", "CharacterData", "Console", "Core", "Css",
                    "Attr", "ApplicationCache", "DOMImplementation", "MimeType", "MimeTypeArray",
                    "DOMNamedFlowCollection", "Plugin", "PluginArray", "Range", "Screen", "SecurityPolicy",
                    "Selection", "DOMSettableTokenList", "DOMStringList", "DOMTokenList",
                    "Window", "CSS", "Document", "DocumentFragment", "DocumentType",
                    "Element", "EntityReference", "Enums", "Event", "EventTarget", "EventTargetClosures", "Events", "File",
                    "FileList", "Geolocation", "HTMLAnchorElement", "HTMLAppletElement",
                    "HTMLAreaElement", "HTMLAudioElement", "HTMLBRElement", "HTMLBaseElement", "HTMLBaseFontElement",
                    "HTMLBodyElement", "HTMLButtonElement", "HTMLCanvasElement", "HTMLCollection",
                    "HTMLDListElement", "HTMLDetailsElement", "HTMLDirectoryElement",
                    "HTMLDivElement", "HTMLDocument", "HTMLElement", "HTMLEmbedElement",
                    "HTMLFieldSetElement", "HTMLFontElement", "HTMLFormElement", "HTMLFrameElement",
                    "HTMLFrameSetElement", "HTMLHRElement", "HTMLHeadElement", "HTMLHeadingElement",
                    "HTMLHtmlElement", "HTMLIFrameElement", "HTMLImageElement", "HTMLInputElement",
                    "HTMLKeygenElement", "HTMLLIElement", "HTMLLabelElement", "HTMLLegendElement",
                    "HTMLLinkElement", "HTMLMapElement", "HTMLMarqueeElement", "HTMLMediaElement",
                    "HTMLMenuElement", "HTMLMetaElement", "HTMLModElement", "HTMLOListElement",
                    "HTMLObjectElement", "HTMLOptGroupElement", "HTMLOptionElement",
                    "HTMLOptionsCollection", "HTMLParagraphElement", "HTMLParamElement",
                    "HTMLPreElement", "HTMLQuoteElement", "HTMLScriptElement", "HTMLSelectElement",
                    "HTMLStyleElement", "HTMLTableCaptionElement", "HTMLTableCellElement",
                    "HTMLTableColElement", "HTMLTableElement", "HTMLTableRowElement",
                    "HTMLTableSectionElement", "HTMLTextAreaElement", "HTMLTitleElement",
                    "HTMLUListElement", "HTMLVideoElement", "History", "Html", "KeyboardEvent",
                    "Location", "MediaError", "MediaList", "MediaQueryList", "MessagePort", "MouseEvent",
                    "NamedNodeMap", "Navigator", "Node", "NodeFilter",
                    "NodeIterator", "NodeList", "Offline", "Performance", "PerformanceNavigation", "PerformanceTiming", "ProcessingInstruction",
                    "Range", "Ranges", "Screen", "Storage", "StorageInfo", "StorageQuota", "StyleMedia",
                    "StyleSheet", "StyleSheetList", "Stylesheets", "Text", "TextTrack", "TextTrackCue", "TextTrackCueList", "TextTrackList", "TimeRanges", "Touch",
                    "Traversal", "TreeWalker", "UIEvent", "ValidityState", "VideoTrack", "VideoTrackList", "View", "WebKitAnimation",
                    "WebKitAnimationList", "WebKitNamedFlow", "WebKitPoint", "WheelEvent",
                    "XPathExpression", "XPathNSResolver", "XPathResult", "Xml", "Xpath"])

fixEventName "FileReader" "onabort" = "AbortEvent"
fixEventName "HTMLMediaElement" "onpause" = "PauseEvent"
fixEventName "HTMLMediaElement" "onplay" = "PlayEvent"
fixEventName "IDBDatabase" "onclose" = "CloseEvent"
fixEventName "IDBTransaction" "onabort" = "AbortEvent"
fixEventName "MediaStream" "onaddtrack" = "AddTrackEvent"
fixEventName "MediaStream" "onremovetrack" = "RemoveTrackEvent"
fixEventName "Notification" "onclose" = "CLoseEvent"
fixEventName "Notification" "onshow" = "ShowEvent"
fixEventName "RTCDataChannel" "onclose" = "CloseEvent"
fixEventName "RTCPeerConnection" "onaddstream" = "AddStreamEvent"
fixEventName "RTCPeerConnection" "onremovestream" = "RemoveStreamEvent"
fixEventName "SourceBuffer" "onabort" = "AbortEvent"
fixEventName "WebSocket" "onclose" = "CloseEvent"
fixEventName "XMLHttpRequestEventTarget" "onabort" = "AbortEvent"
fixEventName _ ('o':'n':x) = fixEventName' x
fixEventName i x = error $ "Event that does not start with 'On' : " ++ x ++ " in " ++ i

fixEventName' ('w':'e':'b':'k':'i':'t':x) = "WebKit" ++ fixEventName' x
fixEventName' "addstream" = "AddStream"
fixEventName' "addtrack" = "AddTrack"
fixEventName' "audioprocess" = "AudioProcess"
fixEventName' "afterprint" = "AfterPrint"
fixEventName' "beforecopy" = "BeforeCopy"
fixEventName' "beforecut" = "BeforeCut"
fixEventName' "beforepaste" = "BeforePaste"
fixEventName' "beforeunload" = "BeforeUnload"
fixEventName' "canplay" = "CanPlay"
fixEventName' "canplaythrough" = "CanPlayThrough"
fixEventName' "chargingchange" = "ChargingChange"
fixEventName' "chargingtimechange" = "ChargingTimeChange"
fixEventName' "contextmenu" = "ContextMenu"
fixEventName' "cuechange" = "CueChange"
fixEventName' "currentplaybacktargetiswirelesschanged" = "CurrentPlaybackTargetIsWirelessChanged"
fixEventName' "datachannel" = "DataChannel"
fixEventName' "dblclick" = "DblClick"
fixEventName' "devicemotion" = "DeviceMotion"
fixEventName' "deviceorientation" = "DeviceOrientation"
fixEventName' "deviceproximity" = "DeviceProximity"
fixEventName' "dischargingtimechange" = "DischargingTimeChange"
fixEventName' "durationchange" = "DurationChange"
fixEventName' "fullscreenchange" = "FullscreenChange"
fixEventName' "fullscreenerror" = "FullscreenError"
fixEventName' "gesturechange" = "GestureChange"
fixEventName' "gestureend" = "GestureEnd"
fixEventName' "gesturestart" = "GestureStart"
fixEventName' "hashchange" = "HashChange"
fixEventName' "icecandidate" = "IceCandidate"
fixEventName' "iceconnectionstatechange" = "IceConnectionStateChange"
fixEventName' "keyadded" = "KeyAdded"
fixEventName' "keydown" = "KeyDown"
fixEventName' "keyerror" = "KeyError"
fixEventName' "keymessage" = "KeyMessage"
fixEventName' "keypress" = "KeyPress"
fixEventName' "keyup" = "KeyUp"
fixEventName' "levelchange" = "LevelChange"
fixEventName' "loadeddata" = "LoadedData"
fixEventName' "loadedmetadata" = "LoadedMetadata"
fixEventName' "loadend" = "LoadEnd"
fixEventName' "loadingdone" = "LoadingDone"
fixEventName' "loadstart" = "LoadStart"
fixEventName' "needkey" = "NeedKey"
fixEventName' "negotiationneeded" = "NegotiationNeeded"
fixEventName' "noupdate" = "NoUpdate"
fixEventName' "orientationchange" = "OrientationChange"
fixEventName' "overconstrained" = "OverConstrained"
fixEventName' "pagehide" = "PageHide"
fixEventName' "pageshow" = "PageShow"
fixEventName' "playbacktargetavailabilitychanged" = "PlaybackTargetAvailabilityChanged"
fixEventName' "presentationmodechanged" = "PresentationModeChanged"
fixEventName' "popstate" = "PopState"
fixEventName' "ratechange" = "RateChange"
fixEventName' "readystatechange" = "ReadyStateChange"
fixEventName' "removestream" = "RemoveStream"
fixEventName' "removetrack" = "RemoveTrack"
fixEventName' "resourcetimingbufferfull" = "ResourceTimingBufferFull"
fixEventName' "selectstart" = "SelectStart"
fixEventName' "signalingstatechange" = "SignalingStateChange"
fixEventName' "timeupdate" = "TimeUpdate"
fixEventName' "tonechange" = "ToneChange"
fixEventName' "transitionend" = "TransitionEnd"
fixEventName' "updateready" = "UpdateReady"
fixEventName' "upgradeneeded" = "UpgradeNeeded"
fixEventName' "versionchange" = "VersionChange"
fixEventName' "volumechange" = "VolumeChange"
fixEventName' "willrevealbottom" = "WillRevealBottom"
fixEventName' "willrevealleft" = "WillRevealLeft"
fixEventName' "willrevealright" = "WillRevealRight"
fixEventName' "willrevealtop" = "WillRevealTop"
fixEventName' ('a':'n':'i':'m':'a':'t':'i':'o':'n':x) = 'A':'n':'i':'m':'a':'t':'i':'o':'n':U.toUpperHead x
fixEventName' ('m':'o':'u':'s':'e':x) = 'M':'o':'u':'s':'e':U.toUpperHead x
fixEventName' ('t':'o':'u':'c':'h':x) = 'T':'o':'u':'c':'h':U.toUpperHead x
fixEventName' ('d':'r':'a':'g':x) = 'D':'r':'a':'g':U.toUpperHead x
-- fixEventName' x = trace ("fixEventName' \""++x++"\" = \"" ++ U.toUpperHead x ++ "\"") x
fixEventName' x = U.toUpperHead x

eventType "XMLHttpRequestEventTarget" "onabort" = "XMLHttpRequestProgressEvent"
eventType i "onabort" | "IDB" `isPrefixOf` i  = "Event"
                      | otherwise             = "UIEvent"
eventType _ "onafterprint"                    = "Event"
eventType _ "onanimationstart"                = "AnimationEvent"
eventType _ "onanimationiteration"            = "AnimationEvent"
eventType _ "onanimationend"                  = "AnimationEvent"
eventType _ "onwebkitanimationend"            = "AnimationEvent"
eventType _ "onwebkitanimationiteration"      = "AnimationEvent"
eventType _ "onwebkitanimationstart"          = "AnimationEvent"
eventType _ "onaudioprocess" = "AudioProcessingEvent"
eventType _ "onbeforeprint" = "Event"
eventType _ "onbeforeunload" = "BeforeUnloadEvent"
eventType _ "onbeginEvent" = "TimeEvent"
eventType _ "onblocked" = "IDBVersionChangeEvent"
eventType _ "onblur" = "FocusEvent"
eventType _ "oncached" = "Event"
eventType _ "oncanplay" = "Event"
eventType _ "oncanplaythrough" = "Event"
eventType _ "onchange" = "Event"
eventType _ "onchargingchange" = "Event"
eventType _ "onchargingtimechange" = "Event"
eventType _ "onchecking" = "Event"
eventType _ "onclick" = "MouseEvent"
eventType _ "onclose" = "CloseEvent"
eventType _ "oncompassneedscalibration" = "Unimplemented"
eventType i "oncomplete" | "IDB" `isPrefixOf` i  = "Event"
                         | otherwise = "OfflineAudioCompletionEvent"
eventType _ "oncompositionend" = "CompositionEvent"
eventType _ "oncompositionstart" = "CompositionEvent"
eventType _ "oncompositionupdate" = "CompositionEvent"
eventType _ "oncontextmenu" = "MouseEvent"
eventType _ "oncopy" = "ClipboardEvent"
eventType _ "oncut" = "ClipboardEvent"
eventType _ "ondblclick" = "MouseEvent"
eventType _ "ondevicelight" = "DeviceLightEvent"
eventType _ "ondevicemotion" = "DeviceMotionEvent"
eventType _ "ondeviceorientation" = "DeviceOrientationEvent"
eventType _ "onwebkitdeviceproximity" = "DeviceProximityEvent"
eventType _ "ondeviceproximity" = "DeviceProximityEvent"
eventType _ "ondischargingtimechange" = "Event"
eventType _ "onDOMContentLoaded" = "Event"
eventType _ "ondownloading" = "Event"
eventType _ "ondrag" = "MouseEvent" -- Mozilla has a DragEvent interface
eventType _ "ondragend" = "MouseEvent" -- Mozilla has a DragEvent interface
eventType _ "ondragenter" = "MouseEvent" -- Mozilla has a DragEvent interface
eventType _ "ondragleave" = "MouseEvent" -- Mozilla has a DragEvent interface
eventType _ "ondragover" = "MouseEvent" -- Mozilla has a DragEvent interface
eventType _ "ondragstart" = "MouseEvent" -- Mozilla has a DragEvent interface
eventType _ "ondrop" = "MouseEvent" -- Mozilla has a DragEvent interface
eventType _ "ondurationchange" = "Event"
eventType _ "onemptied" = "Event"
eventType _ "onended" = "Event"
eventType _ "onendEvent" = "TimeEvent"
eventType "Document"                  "onerror"    = "UIEvent"
eventType "Element"                   "onerror"    = "UIEvent"
eventType "XMLHttpRequestEventTarget" "onerror"    = "XMLHttpRequestProgressEvent"
eventType i "onerror" | "IDB" `isPrefixOf` i  = "Event"
                      | otherwise             = "UIEvent"
eventType _ "onfocus" = "FocusEvent"
eventType _ "onfocusinUnimplemented" = "see"
eventType _ "onfocusoutUnimplemented" = "see"
eventType _ "onfullscreenchange" = "Event"
eventType _ "onfullscreenerror" = "Event"
eventType _ "ongamepadconnected" = "GamepadEvent"
eventType _ "ongamepaddisconnected" = "GamepadEvent"
eventType _ "ongesturestart" = "UIEvent"
eventType _ "ongesturechange" = "UIEvent"
eventType _ "ongestureend" = "UIEvent"
eventType _ "onhashchange" = "HashChangeEvent"
eventType _ "oninput" = "Event"
eventType _ "oninvalid" = "Event"
eventType _ "onkeydown" = "KeyboardEvent"
eventType _ "onkeypress" = "KeyboardEvent"
eventType _ "onkeyup" = "KeyboardEvent"
eventType _ "onlanguagechange" = "Event"
eventType _ "onThe" = "definition"
eventType _ "onlevelchange" = "Event"
eventType "XMLHttpRequestEventTarget" "onload" = "XMLHttpRequestProgressEvent"
eventType _                           "onload" = "UIEvent"
eventType _ "onloadeddata" = "Event"
eventType _ "onloadedmetadata" = "Event"
eventType _ "onloadend" = "ProgressEvent"
eventType _ "onloadstart" = "ProgressEvent"
eventType _ "onmessage" = "MessageEvent"
eventType _ "onmousedown" = "MouseEvent"
eventType _ "onmouseenter" = "MouseEvent"
eventType _ "onmouseleave" = "MouseEvent"
eventType _ "onmousemove" = "MouseEvent"
eventType _ "onmouseout" = "MouseEvent"
eventType _ "onmouseover" = "MouseEvent"
eventType _ "onmouseup" = "MouseEvent"
eventType _ "onnoupdate" = "Event"
eventType _ "onobsolete" = "Event"
eventType _ "onoffline" = "Event"
eventType _ "ononline" = "Event"
eventType _ "onopen" = "Event"
eventType _ "onorientationchange" = "Event"
eventType _ "onpagehide" = "PageTransitionEvent"
eventType _ "onpageshow" = "PageTransitionEvent"
eventType _ "onpaste" = "ClipboardEvent"
eventType _ "onpause" = "Event"
eventType _ "onpointerlockchange" = "Event"
eventType _ "onpointerlockerror" = "Event"
eventType _ "onplay" = "Event"
eventType _ "onplaying" = "Event"
eventType _ "onpopstate" = "PopStateEvent"
eventType "XMLHttpRequestEventTarget" "onprogress" = "XMLHttpRequestProgressEvent"
eventType _                           "onprogress" = "ProgressEvent"
eventType _ "onratechange" = "Event"
eventType _ "onreadystatechange" = "Event"
eventType _ "onrepeatEvent" = "TimeEvent"
eventType _ "onreset" = "Event"
eventType _ "onresize" = "UIEvent"
eventType _ "onscroll" = "UIEvent"
eventType _ "onseeked" = "Event"
eventType _ "onseeking" = "Event"
eventType _ "onselect" = "UIEvent"
eventType _ "onshow" = "MouseEvent"
eventType _ "onstalled" = "Event"
eventType _ "onstorage" = "StorageEvent"
eventType _ "onsubmit" = "Event"
eventType _ "onsuccess" = "Event"
eventType _ "onsuspend" = "Event"
eventType _ "onSVGAbort" = "SVGEvent"
eventType _ "onSVGError" = "SVGEvent"
eventType _ "onSVGLoad" = "SVGEvent"
eventType _ "onSVGResize" = "SVGEvent"
eventType _ "onSVGScroll" = "SVGEvent"
eventType _ "onSVGUnload" = "SVGEvent"
eventType _ "onSVGZoom" = "SVGZoomEvent"
eventType _ "ontimeout" = "ProgressEvent"
eventType _ "ontimeupdate" = "Event"
eventType _ "ontouchcancel" = "TouchEvent"
eventType _ "ontouchend" = "TouchEvent"
eventType _ "ontouchenter" = "TouchEvent"
eventType _ "ontouchleave" = "TouchEvent"
eventType _ "ontouchmove" = "TouchEvent"
eventType _ "ontouchstart" = "TouchEvent"
eventType _ "onwebkittransitionend" = "TransitionEvent"
eventType _ "ontransitionend" = "TransitionEvent"
eventType _ "onunload" = "UIEvent"
eventType _ "onupdateready" = "Event"
eventType _ "onupgradeneeded" = "IDBVersionChangeEvent"
eventType _ "onuserproximity" = "SensorEvent"
eventType _ "onversionchange" = "IDBVersionChangeEvent"
eventType _ "onvisibilitychange" = "Event"
eventType _ "onvolumechange" = "Event"
eventType _ "onwaiting" = "Event"
eventType _ "onwheel" = "WheelEvent"
eventType _ "onaddtrack" = "Event"
eventType _ "onremovetrack" = "Event"
eventType _ "onmousewheel" = "MouseEvent"
eventType _ "onsearch" = "Event"
eventType _ "onwebkitwillrevealbottom" = "Event"
eventType _ "onwebkitwillrevealleft" = "Event"
eventType _ "onwebkitwillrevealright" = "Event"
eventType _ "onwebkitwillrevealtop" = "Event"
eventType _ "onbeforecut" = "ClipboardEvent"
eventType _ "onbeforecopy" = "ClipboardEvent"
eventType _ "onbeforepaste" = "ClipboardEvent"
eventType _ "onselectstart" = "Event"
eventType _ "onwebkitfullscreenchange" = "Event"
eventType _ "onwebkitfullscreenerror" = "Event"
eventType _ "onloading" = "Event"
eventType _ "onloadingdone" = "Event"
eventType _ "onwebkitkeyadded" = "Event"
eventType _ "onwebkitkeyerror" = "Event"
eventType _ "onwebkitkeymessage" = "Event"
eventType _ "onwebkitneedkey" = "Event"
eventType _ "onwebkitcurrentplaybacktargetiswirelesschanged" = "Event"
eventType _ "onwebkitplaybacktargetavailabilitychanged" = "Event"
eventType _ "onactive" = "Event"
eventType _ "oninactive" = "Event"
eventType _ "onmute" = "Event"
eventType _ "onunmute" = "Event"
eventType _ "onstarted" = "Event"
eventType _ "onoverconstrained" = "Event"
eventType _ "onwebkitresourcetimingbufferfull" = "Event"
eventType _ "ontonechange" = "Event"
eventType _ "onnegotiationneeded" = "Event"
eventType _ "onicecandidate" = "RTCIceCandidateEvent"
eventType _ "onsignalingstatechange" = "Event"
eventType _ "onaddstream" = "Event"
eventType _ "onremovestream" = "Event"
eventType _ "oniceconnectionstatechange" = "Event"
eventType _ "ondatachannel" = "Event"
eventType _ "onconnect" = "Event"
eventType _ "onstart" = "Event"
eventType _ "onend" = "Event"
eventType _ "onresume" = "Event"
eventType _ "onmark" = "Event"
eventType _ "onboundary" = "Event"
eventType _ "oncuechange" = "Event"
eventType _ "onenter" = "Event"
eventType _ "onexit" = "Event"
eventType _ "onwebkitpresentationmodechanged" = "Event"
eventType i e                                 = trace ("Please add:\neventType _ \"" ++ e ++ "\" = \"Event\"") e

paramName (I.Param _ (I.Id p) _ _ _) = paramName' p

paramName' "data"    = "data'"
paramName' "pattern" = "pattern'"
paramName' "type"    = "type'"
paramName' "where"   = "where'"
paramName' "family"  = "family'"
paramName' p = p

getEnums (I.TypeDecl (I.TyEnum (Just (I.Id typename)) _)) = [typename]
getEnums (I.Module _ defs) = concatMap getEnums defs
getEnums _ = []

getAllInterfaces (I.Interface (I.Id name) _ _ _ _) = [jsname' name]
getAllInterfaces (I.Module _ defs) = concatMap getAllInterfaces defs
getAllInterfaces _ = []

getParents (I.Interface _ names _ _ _) = map jsname' names
getParents (I.Module _ defs) = concatMap getParents defs
getParents (I.Implements _ (I.Id i)) = [jsname' i]
getParents _ = []

disambiguate "WebGLRenderingContextBase" a b = disambiguate "WebGLRenderingContext" a b
disambiguate "CSS" "supports" [_, _] = "2"
disambiguate "CSSStyleSheet" "insertRule" [_, _] = "Deprecated"
disambiguate "HTMLInputElement" "setRangeText" [_, _, _, _] = "4"
disambiguate "HTMLTextAreaElement" "setRangeText" [_, _, _, _] = "4"
disambiguate "Navigator" "vibrate" [I.Param _ _ (I.TySequence _ _) _ _] = "Pattern"
disambiguate "ApplePaySession" "completeShippingMethodSelection" [I.Param _ _ (I.TyName "ApplePayShippingMethodUpdate" _) _ _] = "Update"
disambiguate "ApplePaySession" "completeShippingContactSelection" [I.Param _ _ (I.TyName "ApplePayShippingContactUpdate" _) _ _] = "Update"
disambiguate "ApplePaySession" "completePaymentMethodSelection" [I.Param _ _ (I.TyName "ApplePayPaymentMethodUpdate" _) _ _] = "Update"
disambiguate "ApplePaySession" "completePayment" [I.Param _ _ (I.TyName "ApplePayPaymentAuthorizationResult" _) _ _] = "Result"
disambiguate "AudioContext" "createBuffer" [_, _] = "FromArrayBuffer"
disambiguate "AudioNode" "connect" [I.Param _ _ (I.TyName "AudioParam" _) _ _, _] = "Param"
disambiguate "CanvasRenderingContext2D" name (I.Param _ _ (I.TyName "Path2D" _) _ _:_) | name `elem` canvasPathFunctionNames = "Path"
disambiguate "CanvasRenderingContext2D" "setStrokeColor" (I.Param _ (I.Id "grayLevel") _ _ _:_) = "Gray"
disambiguate "CanvasRenderingContext2D" "setStrokeColor" (I.Param _ (I.Id "r") _ _ _:_) = "RGB"
disambiguate "CanvasRenderingContext2D" "setStrokeColor" (I.Param _ (I.Id "c") _ _ _:_) = "CYMK"
disambiguate "CanvasRenderingContext2D" "setFillColor" (I.Param _ (I.Id "grayLevel") _ _ _:_) = "Gray"
disambiguate "CanvasRenderingContext2D" "setFillColor" (I.Param _ (I.Id "r") _ _ _:_) = "RGB"
disambiguate "CanvasRenderingContext2D" "setFillColor" (I.Param _ (I.Id "c") _ _ _:_) = "CYMK"
disambiguate "CanvasRenderingContext2D" "setShadow" (_:_:_:I.Param _ (I.Id "grayLevel") _ _ _:_) = "Gray"
disambiguate "CanvasRenderingContext2D" "setShadow" (_:_:_:I.Param _ (I.Id "r") _ _ _:_) = "RGB"
disambiguate "CanvasRenderingContext2D" "setShadow" (_:_:_:I.Param _ (I.Id "c") _ _ _:_) = "CYMK"
disambiguate "CanvasRenderingContext2D" "drawImage" [I.Param _ (I.Id "canvas") _ _ _,_,_] = "FromCanvas"
disambiguate "CanvasRenderingContext2D" "drawImage" [I.Param _ (I.Id "canvas") _ _ _,_,_,_,_] = "FromCanvasScaled"
disambiguate "CanvasRenderingContext2D" "drawImage" [I.Param _ (I.Id "canvas") _ _ _,_,_,_,_,_,_,_,_] = "FromCanvasPart"
disambiguate "CanvasRenderingContext2D" "drawImage" [I.Param _ (I.Id "video") _ _ _,_,_] = "FromVideo"
disambiguate "CanvasRenderingContext2D" "drawImage" [I.Param _ (I.Id "video") _ _ _,_,_,_,_] = "FromVideoScaled"
disambiguate "CanvasRenderingContext2D" "drawImage" [I.Param _ (I.Id "video") _ _ _,_,_,_,_,_,_,_,_] = "FromVideoPart"
disambiguate "CanvasRenderingContext2D" "drawImage" [_,_,_] = ""
disambiguate "CanvasRenderingContext2D" "drawImage" [_,_,_,_,_] = "Scaled"
disambiguate "CanvasRenderingContext2D" "drawImage" [_,_,_,_,_,_,_,_,_] = "Part"
disambiguate "CanvasRenderingContext2D" "putImageData" [_,_,_,_,_,_,_] = "Dirty"
disambiguate "CanvasRenderingContext2D" "webkitPutImageDataHD" [_,_,_,_,_,_,_] = "Dirty"
disambiguate "CanvasRenderingContext2D" "createPattern" [I.Param _ (I.Id "canvas") _ _ _,_] = "FromCanvas"
disambiguate "CanvasRenderingContext2D" "createImageData" [_,_] = "Size"
disambiguate "CanvasRenderingContext2D" "setLineWidth" _ = "Function"
disambiguate "CanvasRenderingContext2D" "setLineCap" _ = "Function"
disambiguate "CanvasRenderingContext2D" "setLineJoin" _ = "Function"
disambiguate "CanvasRenderingContext2D" "setMiterLimit" _ = "Function"
disambiguate "DOMCSSNamespace" "supports" [_] = "Condition"
disambiguate "DataTransferItemList" "add" [_] = "File"
disambiguate "Element" "scroll" [_] = "Opt"
disambiguate "Element" "scrollTo" [_] = "Opt"
disambiguate "Element" "scrollBy" [_] = "Opt"
disambiguate "FormData" "append" [_,_,_] = "Blob"
disambiguate "HTMLCanvasElement" "toBlob" _ = "'"
disambiguate "HTMLFormElement" "get" [I.Param _ _ (I.TyName "DOMString" _) _ _] = ""
disambiguate "HTMLFormElement" "get" _ = "At"
disambiguate "HTMLSelectElement" "add" [_, I.Param _ (I.Id "before") _ _ _] = "Before"
disambiguate "HTMLSelectElement" "remove" [] = "This"
disambiguate "HTMLSelectElement" "remove" [I.Param _ _ (I.TyName "HTMLOptionElement" _) _ _] = "Element"
disambiguate "HTMLOptionsCollection" "add" [_, I.Param _ (I.Id "before") _ _ _] = "Before"
disambiguate "HTMLOptionsCollection" "remove" [I.Param _ _ (I.TyName "HTMLOptionElement" _) _ _] = "Element"
disambiguate "IDBIndex" _ (I.Param _ _ (I.TyOptional (I.TyName "IDBKeyRange" _)) _ _:_) = "Range"
disambiguate "IDBObjectStore" _ (I.Param _ _ (I.TyOptional (I.TyName "IDBKeyRange" _)) _ _:_) = "Range"
disambiguate "KeyboardEvent" "initKeyboardEvent" [_,_,_,_,_,_,_,_,_,_] = "'"
disambiguate "MediaControlsHost" "sortedTrackListForMenu" [I.Param _ _ (I.TyName "AudioTrackList" _) _ _] = "Audio"
disambiguate "MediaControlsHost" "displayNameForTrack" [I.Param _ _ (I.TyName "AudioTrack" _) _ _] = "Audio"
disambiguate "Path2D" "addPath" [_, _] = "WithTransform"
disambiguate "RTCDataChannel" "send" [I.Param _ _ (I.TyName "ArrayBufferView" _) _ _] = "View"
disambiguate "RTCDataChannel" "send" [I.Param _ _ (I.TyName "Blob" _) _ _] = "Blob"
disambiguate "RTCDataChannel" "send" [I.Param _ _ (I.TyName "DOMString" _) _ _] = "String"
disambiguate "RTCDataChannel" "send" [I.Param _ _ (I.TyName "USVString" _) _ _] = "String"
disambiguate "RTCPeerConnection" "addTransceiver" [I.Param _ (I.Id "track") _ _ _, _] = "Track"
disambiguate "SourceBuffer" "appendBuffer" [I.Param _ _ (I.TyName "ArrayBufferView" _) _ _] = "View"
disambiguate "URL" "createObjectURL" [I.Param _ _ (I.TyName "MediaSource" _) _ _] = "Source"
disambiguate "URL" "createObjectURL" [I.Param _ _ (I.TyName "MediaStream" _) _ _] = "Stream"
disambiguate "WebSocket" "send" [I.Param _ _ (I.TyName "ArrayBufferView" _) _ _] = "View"
disambiguate "WebSocket" "send" [I.Param _ _ (I.TyName "Blob" _) _ _] = "Blob"
disambiguate "WebSocket" "send" [I.Param _ _ (I.TyName "USVString" _) _ _] = "String"
disambiguate "WebGL2RenderingContext" "bufferData" [_, I.Param _ _ (I.TyName "ArrayBufferView" _) _ _, _, _, _] = "View"
disambiguate "WebGL2RenderingContext" "bufferData" [_, I.Param _ _ (I.TyName "GLsizeiptr" _) _ _, _] = "Ptr"
disambiguate "WebGL2RenderingContext" "bufferSubData" [_, _, I.Param _ _ (I.TyName "ArrayBufferView" _) _ _, _, _] = "View"
disambiguate "WebGL2RenderingContext" "getBufferSubData" [_, _, I.Param _ _ (I.TyName "ArrayBufferView" _) _ _] = "View"
disambiguate "WebGL2RenderingContext" "texSubImage3D" [_, _, _, _, _, _, _, _, _, _, I.Param _ _ (I.TyOptional (I.TyName "ArrayBufferView" _)) _ _] = "View"
disambiguate "WebGL2RenderingContext" "texSubImage3D" [_, _, _, _, _, _, _, I.Param _ _ (I.TyOptional (I.TyName "ImageData" _)) _ _] = "Data"
disambiguate "WebGL2RenderingContext" "texSubImage3D" [_, _, _, _, _, _, _, I.Param _ _ (I.TyOptional (I.TyName "HTMLCanvasElement" _)) _ _] = "Canvas"
disambiguate "WebGL2RenderingContext" "texSubImage3D" [_, _, _, _, _, _, _, I.Param _ _ (I.TyOptional (I.TyName "HTMLVideoElement" _)) _ _] = "Video"
disambiguate "WebGLRenderingContext" "bufferData" [_, I.Param _ _ (I.TyName "GLsizeiptr" _) _ _, _] = "Ptr"
disambiguate "WebGLRenderingContext" "texImage2D" [_, _, _, _, _, _, _, _, I.Param _ _ (I.TyOptional (I.TyName "ArrayBufferView" _)) _ _] = "View"
disambiguate "WebGLRenderingContext" "texImage2D" [_, _, _, _, _, I.Param _ _ (I.TyOptional (I.TyName "ImageData" _)) _ _] = "Data"
disambiguate "WebGLRenderingContext" "texImage2D" [_, _, _, _, _, I.Param _ _ (I.TyOptional (I.TyName "HTMLCanvasElement" _)) _ _] = "Canvas"
disambiguate "WebGLRenderingContext" "texImage2D" [_, _, _, _, _, I.Param _ _ (I.TyOptional (I.TyName "HTMLVideoElement" _)) _ _] = "Video"
disambiguate "WebGLRenderingContext" "texSubImage2D" [_, _, _, _, _, _, _, _, I.Param _ _ (I.TyOptional (I.TyName "ArrayBufferView" _)) _ _] = "View"
disambiguate "WebGLRenderingContext" "texSubImage2D" [_, _, _, _, _, _, I.Param _ _ (I.TyOptional (I.TyName "ImageData" _)) _ _] = "Data"
disambiguate "WebGLRenderingContext" "texSubImage2D" [_, _, _, _, _, _, I.Param _ _ (I.TyOptional (I.TyName "HTMLCanvasElement" _)) _ _] = "Canvas"
disambiguate "WebGLRenderingContext" "texSubImage2D" [_, _, _, _, _, _, I.Param _ _ (I.TyOptional (I.TyName "HTMLVideoElement" _)) _ _] = "Video"
disambiguate "Window" "alert" [] = "NoMessage"
disambiguate "Window" "scroll" [_] = "Opt"
disambiguate "Window" "scrollTo" [_] = "Opt"
disambiguate "Window" "scrollBy" [_] = "Opt"
disambiguate "XMLHttpRequest" "open" [_,_] = "Simple"
disambiguate _ name _ = ""

exclude "IDBDatabase" "transaction" [I.Param _ _ (I.TyName "DOMStringList" _) _ _, _] = True
exclude _ _ _ = False

canvasPathFunctionNames = [
    "fill",
    "stroke",
    "clip",
    "isPointInPath",
    "isPointInStroke",
    "drawFocusIfNeeded"]

-- | Retrieve a module name as a string from Module
modName :: H.Module -> String
modName m = read $ drop 1 $ dropWhile (not . isSpace) (show m)

-- | Get a module namespace (all elements of name separated with dots except
-- the last one)
modNS :: String -> String
modNS mn = intercalate "." . reverse . drop 1 . reverse $ parts (== '.') mn

webkitTypeGuard "DOMNamedFlowCollection" = "webkitgtk-2.2"
webkitTypeGuard "SecurityPolicy" =  "webkitgtk-1.10"
webkitTypeGuard "WindowCSS" = "webkitgtk-2.2"
webkitTypeGuard "KeyboardEvent" = "webkitgtk-2.2"
webkitTypeGuard "StorageInfo" = "webkitgtk-1.10"
webkitTypeGuard "AudioTrack" = "webkitgtk-2.2"
webkitTypeGuard "AudioTrackList" = "webkitgtk-2.2"
webkitTypeGuard "BarProp" = "webkitgtk-2.2"
webkitTypeGuard "BatteryManager" = "webkitgtk-2.2"
webkitTypeGuard "CSS" = "webkitgtk-2.2"
webkitTypeGuard "Performance" = "webkitgtk-2.2"
webkitTypeGuard "PerformanceNavigation" = "webkitgtk-2.2"
webkitTypeGuard "PerformanceTiming" = "webkitgtk-2.2"
webkitTypeGuard "StorageQuota" = "webkitgtk-2.2"
webkitTypeGuard "TextTrack" = "webkitgtk-2.2"
webkitTypeGuard "TextTrackCue" = "webkitgtk-2.2"
webkitTypeGuard "TextTrackCueList" = "webkitgtk-2.2"
webkitTypeGuard "TextTrackList" = "webkitgtk-2.2"
webkitTypeGuard "Touch" = "webkitgtk-2.4"
webkitTypeGuard "VideoTrack" = "webkitgtk-2.2"
webkitTypeGuard "VideoTrackList" = "webkitgtk-2.2"
webkitTypeGuard "WheelEvent" = "webkitgtk-2.4"
webkitTypeGuard _ = "webkit-dom"

sumTypesList =
   [ ([I.TyName "CanvasPattern" Nothing, I.TyName "CanvasGradient" Nothing, I.TyName "DOMString" Nothing], "CanvasStyle")
   , ([I.TyName "DOMString" Nothing, I.TyName "Node" Nothing], "NodeOrString")
   , ([I.TyName "WebGLRenderingContextBase" Nothing, I.TyName "CanvasRenderingContext2D" Nothing], "RenderingContext")
   , ([I.TyName "Bool" Nothing, I.TyName "AddEventListenerOptions" Nothing], "AddEventListenerOptionsOrBool")
   , ([I.TyName "Bool" Nothing, I.TyName "EventListenerOptions" Nothing], "EventListenerOptionsOrBool")
   , ([I.TyName "ArrayBuffer" Nothing, I.TyName "DOMString" Nothing], "StringOrArrayBuffer")
   , ([I.TyName "BinaryData" Nothing, I.TyName "DOMString" Nothing], "StringOrBinaryData")
   , ([I.TyName "BufferSource" Nothing, I.TyName "Blob" Nothing, I.TyName "DOMString" Nothing], "BlobPart")
   , ([I.TyName "Element" Nothing, I.TyName "HTMLCollection" Nothing], "HTMLCollectionOrElement")
   , ([I.TyName "Element" Nothing, I.TyName "RadioNodeList" Nothing], "RadioNodeListOrElement")
   , ([I.TyName "HTMLOptGroupElement" Nothing, I.TyName "HTMLOptionElement" Nothing], "HTMLOptionElementOrGroup")
   , ([I.TyInteger Long, I.TyName "HTMLElement" Nothing], "HTMLElementOrLong")
   , ([I.TyName "IDBIndex" Nothing, I.TyName "IDBObjectStore" Nothing], "IDBCursorSource")
   , ([I.TySequence (I.TyName "DOMString" Nothing) Nothing, I.TyName "DOMString" Nothing], "StringOrStrings")
   , ([I.TyAny, I.TyName "IDBDatabase" Nothing, I.TyName "IDBCursor" Nothing], "IDBRequestResult")
   , ([I.TyName "IDBCursor" Nothing, I.TyName "IDBIndex" Nothing, I.TyName "IDBObjectStore" Nothing], "IDBRequestSource")
   , ([I.TyName "AudioTrack" Nothing, I.TyName "TextTrack" Nothing], "Track")
   , ([I.TyName "TextTrack" Nothing, I.TyName "AudioTrack" Nothing, I.TyName "VideoTrack" Nothing], "Track")
   , ([I.TyName "MessagePort" Nothing, I.TyName "DOMWindow" Nothing], "MessageEventSource")
   , ([I.TyName "JsonWebKey" Nothing, I.TyName "BufferSource" Nothing], "KeyData")
   , ([I.TyName "USVString" Nothing, I.TyRecord (I.TyName "USVString" Nothing) (I.TyName "USVString" Nothing), I.TySequence (I.TySequence (I.TyName "USVString" Nothing) Nothing) Nothing], "URLSearchParamsInit")
   , ([I.TyName "CryptoKeyPair" Nothing, I.TyName "CryptoKey" Nothing], "CryptoKeyOrKeyPair")
   , ([I.TyName "BodyInit" Nothing, I.TyName "Document" Nothing], "XMLHttpRequestBody")
   , ([I.TyName "MediaStream" Nothing, I.TyName "MediaSource" Nothing, I.TyName "Blob" Nothing], "MediaProvider")
   , ([I.TyName "HTMLImageElement" Nothing, I.TyName "HTMLVideoElement" Nothing, I.TyName "HTMLCanvasElement" Nothing], "CanvasImageSource")
   , ([I.TyName "ImageData" Nothing, I.TyName "HTMLImageElement" Nothing, I.TyName "HTMLVideoElement" Nothing, I.TyName "HTMLCanvasElement" Nothing], "TexImageSource")
   , ([I.TyName "USVString" Nothing, I.TyName "DOMFormData" Nothing, I.TyName "BufferSource" Nothing, I.TyName "Blob" Nothing], "BodyInit")
   , ([I.TySequence (I.TyName "GLfloat" Nothing) Nothing, I.TyName "Float32Array" Nothing], "Float32List")
   , ([I.TySequence (I.TyName "GLint" Nothing) Nothing, I.TyName "Int32Array" Nothing], "Int32List")
   , ([I.TyName "RTCIceCandidate" Nothing, I.TyName "RTCIceCandidateInit" Nothing], "RTCIceCandidateOrInit")
   , ([I.TyName "DOMString" Nothing, I.TyName "MediaStreamTrack" Nothing], "MediaStreamTrackOrKind")
   ]

sumTypes = M.fromList sumTypesList

sumTypes' = M.fromListWith (<>) . map swap $ sumTypesList <>
    -- Other sum types but not the default ones
    [ ([I.TySequence (I.TyName "DOMString" Nothing) Nothing, I.TyName "DOMString" Nothing], "IDBKeyPath")
    , ([I.TyOptional (I.TyName "DOMString" Nothing), I.TyFloat Long], "SQLValue")
    , ([I.TyName "ArrayBuffer" Nothing, I.TyName "ArrayBufferView" Nothing], "BinaryData")
    , ([I.TyName "ArrayBuffer" Nothing, I.TyName "ArrayBufferView" Nothing], "BufferSource")
    , ([I.TyName "ArrayBuffer" Nothing, I.TyName "ArrayBufferView" Nothing], "BufferDataSource")
    , ([I.TyName "URLSearchParams" Nothing, I.TyName "DOMFormData" Nothing], "CredentialBodyType")
    ]

sumType x = fromMaybe
              (error $
                 "Unknown sum type please update Common.sumTypes in Common.hs with , ("
                   ++ show x ++ ", \"something\")")
              (M.lookup x sumTypes)

coercibleToJSVal "Bool" = False
coercibleToJSVal t = hasPToJSVal t

hasPToJSVal "Int" = False
hasPToJSVal "Text" = False
hasPToJSVal "String" = False
hasPToJSVal "JSString" = False
hasPToJSVal "(Maybe Text)" = False
hasPToJSVal "(Maybe String)" = False
hasPToJSVal "(Maybe JSString)" = False
hasPToJSVal "[GLfloat]" = False
hasPToJSVal "[GLint]" = False
hasPToJSVal _ = True

hasTo "Float32List" = False
hasTo "Int32List" = False
hasTo _ = True

canBeMaybe (I.TyName "Bool" _) = False
canBeMaybe (I.TyName "boolean" _) = False
canBeMaybe (I.TyName "boolen" _) = False
--canBeMaybe (I.TyInteger _) = False
--canBeMaybe (I.TyApply _ (I.TyInteger _)) = False
--canBeMaybe (I.TyFloat _) = False
canBeMaybe (I.TySequence _ _) = False
canBeMaybe (I.TyOptional (I.TySequence _ _)) = False
canBeMaybe (I.TySum t) = all canBeMaybe t
canBeMaybe _ = True

subTypes :: Map String [String] -> I.Type -> [String]
subTypes childmap (I.TyName "DOMString" Nothing) = ["Text", "JSString", "String"]
subTypes childmap (I.TyName "USVString" Nothing) = ["Text", "JSString", "String"]
subTypes childmap (I.TyName "Bool" Nothing) = ["Bool"]
subTypes childmap (I.TyInteger Long) = ["Int"]
subTypes childmap I.TyAny =["JSVal"]
subTypes childmap (I.TyFloat Long) =["Double"]
subTypes childmap (I.TySequence (I.TyName "DOMString" Nothing) Nothing) = ["[Text]","[JSString]","[String]"]
subTypes childmap (I.TyOptional (I.TyName "DOMString" Nothing)) = ["(Maybe Text)","(Maybe JSString)","(Maybe String)"]
subTypes childmap (I.TySequence (I.TyName x Nothing) Nothing) = ["[" <> typeFor x <> "]"]
subTypes childmap (I.TyRecord (I.TyName "USVString" Nothing) (I.TyName "USVString" Nothing)) = [] -- TODO
subTypes childmap (I.TySequence (I.TySequence (I.TyName "USVString" Nothing) Nothing) Nothing) = ["[[Text]]","[[JSString]]","[[String]]"]
subTypes childmap (I.TyName "BufferSource" Nothing) = ["BinaryData", "BufferSource", "ArrayBufferView", "ArrayBuffer"]
subTypes childmap (I.TyName "BinaryData" Nothing) = ["BinaryData", "BufferSource", "ArrayBufferView", "ArrayBuffer"]
subTypes childmap (I.TyName "BodyInit" Nothing) = ["BodyInit", "Blob", "BinaryData", "BufferSource", "ArrayBufferView", "ArrayBuffer", "FormData", "Text", "JSString", "String"]
subTypes childmap (I.TyName n Nothing) = typeFor n :
    case M.lookup n childmap of
        Nothing -> []
        Just children -> map typeFor children
subTypes _ x = error $ "Add subTypes childmap (" ++ show x ++ ") = [...]"

exportSumTypes :: Map String [String] -> String
exportSumTypes childmap = concatMap defineSumType $ M.toList sumTypes'
  where
    defineSumType (name', types) =
        let name = typeFor name'
            sTypes = nub $ concatMap (subTypes childmap) types
        in
           "  , " ++ name ++ "(" ++ name ++ "), un" ++ name ++ ", Is" ++ name
        ++ (if all hasPToJSVal sTypes
                then ", to" ++ name
                else "") ++ "\n"

defineSumTypes :: Bool -> Map String [String] -> String
defineSumTypes jsffi childmap = concatMap defineSumType $ M.toList sumTypes'
  where
    defineSumType (name', types) =
        let name = typeFor name'
            sTypes = nub $ concatMap (subTypes childmap) types
        in
           "newtype " ++ name ++ " = " ++ name ++ " { un" ++ name ++ " :: JSVal }\n\n"

        ++ "instance PToJSVal " ++ name ++ " where\n"
        ++ "  pToJSVal = un" ++ name ++ "\n"
        ++ "  {-# INLINE pToJSVal #-}\n\n"

        ++ "instance PFromJSVal " ++ name ++ " where\n"
        ++ "  pFromJSVal = " ++ name ++ "\n"
        ++ "  {-# INLINE pFromJSVal #-}\n\n"

        ++ "instance ToJSVal " ++ name ++ " where\n"
        ++ "  toJSVal = return . un" ++ name ++ "\n"
        ++ "  {-# INLINE toJSVal #-}\n\n"

        ++ "instance FromJSVal " ++ name ++ " where\n"
        ++ (if jsffi
                then "  fromJSVal = return . fmap " ++ name ++ " . maybeJSNullOrUndefined\n"
                else "  fromJSVal v = fmap " ++ name ++ " <$> maybeNullOrUndefined v\n")
        ++ "  {-# INLINE fromJSVal #-}\n"
        ++ (if jsffi
                then ""
                else "  fromJSValUnchecked = return . " ++ name ++ "\n"
                  ++ "  {-# INLINE fromJSValUnchecked #-}\n\n")

        ++ (if jsffi
                then ""
                else "instance MakeObject " ++ name ++ " where\n"
                  ++ "  makeObject = makeObject . un" ++ name ++ "\n\n")

        ++ (if all coercibleToJSVal sTypes
                then    "class (FromJSVal o, ToJSVal o, PFromJSVal o, PToJSVal o, Coercible o JSVal) => Is" ++ name ++ " o\n\n"
                     ++ "to" ++ name ++ " :: Is" ++ name ++ " o => o -> " ++ name ++ "\n"
                     ++ "to" ++ name ++ " = " ++ name ++ " . coerce\n\n"
                else if all hasPToJSVal sTypes
                        then   "class (FromJSVal o, ToJSVal o, PToJSVal o) => Is" ++ name ++ " o\n\n"
                             ++ "to" ++ name ++ " :: Is" ++ name ++ " o => o -> " ++ name ++ "\n"
                             ++ "to" ++ name ++ " = " ++ name ++ " . pToJSVal\n\n"
                        else   "class (FromJSVal o, ToJSVal o) => Is" ++ name ++ " o\n\n")

        ++ "instance Is" ++ name ++ " " ++ name ++ "\n"
        ++ concatMap (\subType -> "instance Is" ++ name ++ " " ++ subType ++ "\n") sTypes
        ++ "\n"

-- Tag values corresponding to certain HTML element interfaces

tagFor "HTMLButtonElement" = "button"
tagFor "HTMLDivElement" = "div"
tagFor "HTMLImageElement" = "img"
tagFor "HTMLAppletElement" = "applet"
tagFor "HTMLFontElement" = "font"
tagFor "HTMLFormElement" = "form"
tagFor "HTMLFrameElement" = "frame"
tagFor "HTMLInputElement" = "input"
tagFor "HTMLObjectElement" = "object"
tagFor "HTMLParagraphElement" = "p"
tagFor "HTMLParamElement" = "param"
tagFor "HTMLPreElement" = "pre"
tagFor "HTMLScriptElement" = "script"
tagFor "HTMLTableCellElement" = "td"
tagFor "HTMLTableColElement" = "col"
tagFor "HTMLTableElement" = "table"
tagFor "HTMLTableRowElement" = "tr"
tagFor "HTMLTextAreaElement" = "textarea"
tagFor "HTMLBRElement" = "br"
tagFor "HTMLHRElement" = "hr"
tagFor "HTMLLIElement" = "li"
tagFor "HTMLDListElement" = "dl"
tagFor "HTMLOListElement" = "ol"
tagFor "HTMLUListElement" = "ul"

tagFor _ = ""

-- Fake source location

nullLoc = H.SrcLoc {H.srcFilename = "", H.srcLine = 0, H.srcColumn = 0}

-- A list of single-letter formal argument names (max. 26)

azList = map (: []) ['a' .. 'z']

azHIList = map H.HsIdent azList

-- Rename a module. First character of module name is uppercased. Each
-- underscore followed by a character causes that character uppercased.

renameMod :: String -> String

renameMod "" = ""
renameMod (m:odule) = toUpper m : renameMod' odule where
  renameMod' "" = ""
  renameMod' ('_':o:dule) = '.' : toUpper o : renameMod' dule
  renameMod' ('.':o:dule) = '.' : toUpper o : renameMod' dule
  renameMod' (o:dule) = o : renameMod' dule

-- Convert a name to a type context assertion (assume single parameter class)

name2ctxt name = (mkUIdent $ classFor name, [H.HsTyVar $ head azHIList])

-- A helper function to produce an unqualified identifier

mkUIdent = H.UnQual . H.HsIdent

mkSymbol = H.UnQual . H.HsSymbol

-- A filter to select only operations (methods)

opsOnly :: I.Defn -> Bool
opsOnly I.Operation{} = True
opsOnly _ = False

-- A filter to select only attributes

attrOnly :: I.Defn -> Bool
attrOnly I.Attribute{} = True
attrOnly _ = False

-- A filter to select only interfaces (classes)

visible attr =
        I.ExtAttr (I.Id "PrivateIdentifier") [] `notElem` attr
     || I.ExtAttr (I.Id "PublicIdentifier") [] `elem` attr

intfOnly :: I.Defn -> Bool
intfOnly (I.Interface _ _ _ attr Nothing) = visible attr
intfOnly _ = False

implOnly :: I.Defn -> Bool
implOnly (I.Implements _ _) = True
implOnly _ = False

intfAndCallbacks :: I.Defn -> Bool
intfAndCallbacks (I.Interface _ _ _ attr _) = visible attr
intfAndCallbacks _ = False

-- A filter to select only constant definitions

constOnly :: I.Defn -> Bool
constOnly I.Constant{} = True
constOnly _ = False

-- Collect all operations defined in an interface

collectOps :: I.Defn -> [I.Defn]

collectOps (I.Interface _ _ cldefs _ _) =
  filter opsOnly cldefs

collectOps _ = []

-- Collect all constants defined in an interface

collectConst :: I.Defn -> [I.Defn]

collectConst (I.Interface _ _ cldefs _ _) =
  filter constOnly cldefs

collectConst _ = []

-- Collect all attributes defined in an interface

collectAttrs :: I.Defn -> [I.Defn]

collectAttrs (I.Interface _ _ cldefs _ _) =
  filter attrOnly cldefs

collectAttrs _ = []

-- Declare an instance (very simple case, no context, no methods only one class parameter)

mkInstDecl :: String -> String -> H.HsDecl

mkInstDecl clname typename =
  H.HsInstDecl nullLoc [] (mkUIdent $ classFor clname) [mkTIdent $ typeFor typename] []

-- Build a variable name

mkVar s = H.HsVar $ mkUIdent s

-- Build a method's type signature

mkTsig :: [H.HsType] -> H.HsType -> H.HsType

mkTsig ps a = foldr H.HsTyFun a ps

-- A helper function to produce a type identifier

mkTIdent = H.HsTyVar . H.HsIdent

-- A helper function to produce an export identifier.
-- Datas (Txxx) export all their members.



mkTyList = H.HsTyApp (H.HsTyCon $ H.Special H.HsListCon)

-- Some types pass through as is, other are class names

asIs :: [String] -> (String -> Bool) -> Bool -- ^ Is JSFFI call
    -> String -> Maybe H.HsType
asIs enums isLeaf ffi = asIs' enums isLeaf ffi . typeFor
  where
    asIs' _ _ True "PerformanceEntryList" = Just $ mkTIdent "JSVal"
    asIs' enums _ ffi a  | a `elem` enums = Just $ if ffi then mkTIdent "JSVal" else mkTIdent a
    asIs' _ isLeaf ffi a | isLeaf a       = Just $ mkTIdent a
    asIs' _ _ _ "DOMString"               = Just $ mkTIdent "String"
    asIs' _ _ _ "DOMTimeStamp"            = Just $ mkTIdent "Word"
    asIs' _ _ _ "CompareHow"              = Just $ mkTIdent "Word"
    asIs' _ _ _ x | x `elem` glTypes      = Just $ mkTIdent x
    asIs' _ _ _ "Bool"                    = Just $ mkTIdent "Bool"
    asIs' _ _ _ "bool"                    = Just $ mkTIdent "Bool"
    asIs' _ _ _ "boolen" {-Typo in IDL-}  = Just $ mkTIdent "Bool"
    asIs' _ _ _ "Int"                     = Just $ mkTIdent "Int"
    asIs' _ _ _ "KeyUsage"                = Just $ mkTIdent "CryptoKeyUsage"
    asIs' _ _ _ _                         = Nothing

glTypes = ["GLenum", "GLboolean", "GLbitfield", "GLbyte", "GLshort", "GLint", "GLsizei",
           "GLintptr", "GLsizeiptr", "GLubyte", "GLushort", "GLuint", "GLfloat", "GLclampf",
           "GLint64", "GLuint64"]


makeOptional t@I.TyOptional{} = t
makeOptional t = I.TyOptional t

overrideAttributeType "Node" "parentElement" t = makeOptional t
overrideAttributeType "MouseEvent" "dataTransfer" t = makeOptional t
overrideAttributeType "Window" "location" (I.TyOptional t) = t
overrideAttributeType _ _ t = t

overrideReturnType "FileList" "item" t = makeOptional t
overrideReturnType "DataTransferItem" "getAsFile" t = makeOptional t
overrideReturnType _ _ t = t

-- Validate a map of interface inheritance. Any "Left" parent identifier
-- causes a log message to be produced. It is also checked that an interface
-- does not have itself as a parent. Circular inheritance is not checked.

valParentMap :: Map String [Either String String] -> [String]

valParentMap pm = concat (M.elems m2) where
  m2 = M.mapWithKey lefts pm
  lefts intf = concatMap (leftmsg intf)
  leftmsg intf (Right _) = []
  leftmsg "InternalSettings" _ = []
  leftmsg intf (Left p) = ["Interface " ++ intf ++ " has " ++ p ++ " as a parent, but " ++
                           p ++ " is not defined anywhere"]

-- Prepare a complete map of interfaces inheritance. All ancestors
-- must be defined in the IDL module being processed plus in other
-- modules it includes.

mkParentMap :: [I.Defn] -> Map String [Either String String]

mkParentMap defns = m2 where
  allintfs = nub $ concatMap getintfs defns
  getintfs (I.Module _ moddefs) = filter intfOnly moddefs
  getintfs _ = []
  allimpls = nub $ concatMap getimpls defns
  getimpls (I.Module _ moddefs) = filter implOnly moddefs
  getimpls _ = []
  mimpls = M.fromListWith (<>) [(jsname' a, [jsname' b]) | I.Implements (I.Id a) (I.Id b) <- allimpls]
  m1 = M.fromList $ zip (map getDef allintfs) allintfs
  m2 = M.fromList (map getparents allintfs)
  getparents i@(I.Interface (I.Id n) supers _ at _) = (getDef i, nub . concat $ map parent (map jsname' supers ++ eventTarget n at ++ impls n))
  parent pidf = case M.lookup pidf m1 of
                    Just x -> Right pidf : snd (getparents x)
                    Nothing -> [Left pidf]
  eventTarget name at | name == "EventTarget" = []
                      | I.ExtAttr (I.Id "EventTarget") [] `elem` at = ["EventTarget"]
                      | otherwise = []
  impls name = fromMaybe [] (M.lookup (jsname' name) mimpls)

isStringType (I.TyName "DOMString" Nothing) = True
isStringType (I.TyName "CSSOMString" Nothing) = True
isStringType (I.TyName "ByteString" Nothing) = True
isStringType (I.TyName "USVString" Nothing) = True
isStringType _ = False

isOptionalStringType (I.TyOptional t) = isStringType t
isOptionalStringType _ = False

data CallbackType = SyncContinueAsync | SyncThrowWouldBlock | Async deriving (Show, Eq)
data WrapType = Normal | Underscore | Unsafe | Unchecked deriving (Show, Eq)

wrapName _ [] = error "Empty name passed to wrapName"
wrapName Normal x = x
wrapName Underscore x = x ++ "_"
wrapName Unsafe x = x ++ "Unsafe"
wrapName Unchecked x = x ++ "Unchecked"

callbackPostfix SyncContinueAsync   = ""
callbackPostfix SyncThrowWouldBlock = "Sync"
callbackPostfix Async               = "Async"

-- Obtain a return type signature from a return type
tyRet :: [String] -> Bool -> I.Type -> [I.ExtAttribute] -> WrapType -> Maybe H.HsType
tyRet enums ffi t ext Normal = Just (tyRet' "result" enums ffi t ext)
tyRet enums ffi t ext Underscore = case tyRet' "result" enums ffi t ext of
    H.HsTyTuple [] -> Nothing
    _ -> Just (H.HsTyTuple [])
tyRet enums ffi t ext Unsafe = case tyRet' "result" enums ffi t ext of
    (H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) (H.HsTyApp (H.HsTyCon (H.Special H.HsListCon)) x)) -> Nothing
    (H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) x) -> Just x
    _ -> Nothing
tyRet enums ffi t ext Unchecked = case tyRet' "result" enums ffi t ext of
    (H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) (H.HsTyApp (H.HsTyCon (H.Special H.HsListCon)) x)) -> Nothing
    (H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) x) -> Just x
    _ -> Nothing

tyRet' :: String -> [String] -> Bool -> I.Type -> [I.ExtAttribute] -> H.HsType

tyRet' _ enums ffi (I.TyName "object" Nothing) _
    | ffi = H.HsTyApp (mkTIdent "Nullable") (mkTIdent "GObject")
    | otherwise = H.HsTyApp (mkTIdent "Maybe") (mkTIdent "GObject")
tyRet' pname _ ffi t ext
    | isStringType t && ffi = mkTIdent "JSString"
    | isStringType t = mkTIdent (paramName' pname)
tyRet' _ _ True (I.TyOptional (I.TySequence t _)) _ = mkTIdent "JSVal"
--tyRet ffi (I.TyOptional t@(I.TyName c Nothing)) | isNothing (asIs ffi c) = tyRet ffi t
--tyRet ffi (I.TyOptional t) | otherwise = H.HsTyApp (mkTIdent "Maybe") (tyRet ffi t)
--tyRet' pname enums False (I.TyOptional t@(I.TyName c Nothing)) ext
--    | isNothing (asIs [] (const False) False c) = tyRet' pname enums False t ext
tyRet' pname enums True (I.TyOptional t) ext
    = case tyRet' pname [] True t ext of
            r@(H.HsTyApp (H.HsTyVar (H.HsIdent "Nullable")) _) -> r
            hsType -> H.HsTyApp (mkTIdent "Nullable") hsType
tyRet' pname enums False (I.TyOptional t) ext
    = case tyRet' pname enums False t ext of
            r@(H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) _) -> r
            hsType -> H.HsTyApp (mkTIdent "Maybe") hsType
--tyRet ffi (I.TySafeArray t) | ffi = H.HsTyApp (mkTIdent "JSVal") (mkTyList $ tyRet ffi t)
--                            | otherwise = mkTyList (tyRet ffi t)
--tyRet ffi (I.TySequence t _)| ffi = H.HsTyApp (mkTIdent "JSVal") (mkTyList $ tyRet ffi t)
--                            | otherwise = mkTyList (tyRet ffi t)
tyRet' pname enums False (I.TySafeArray t) ext = mkTyList (tyRet' pname enums False t ext)
tyRet' pname enums False (I.TyFrozenArray t) ext = mkTyList (tyRet' pname enums False t ext)
tyRet' pname enums False (I.TySequence t _) ext = mkTyList (tyRet' pname enums False t ext)
tyRet' _ _ True (I.TyName "GLintptr" Nothing) _ = mkTIdent "Double"
tyRet' _ _ True (I.TyName "GLsizeiptr" Nothing) _ = mkTIdent "Double"
tyRet' _ _ True (I.TyName "GLint64" Nothing) _ = mkTIdent "Double"
tyRet' _ _ True (I.TyName "GLuint64" Nothing) _ = mkTIdent "Double"
tyRet' _ _ _ (I.TyName "TypedArray" Nothing) _ = mkTIdent $ typeFor "RawTypedArray"
tyRet' _ enums ffi (I.TyName c Nothing) _ = case asIs enums (const False) ffi c of
  Nothing | ffi -> mkTIdent $ typeFor c
  Nothing -> mkTIdent $ typeFor c
  Just c' -> c'
tyRet' _ _ _ I.TyVoid _ = H.HsTyTuple []
tyRet' _ _ ffi (I.TyInteger LongLong) _ | ffi = mkTIdent "Double"
                                        | otherwise = mkTIdent "Int64"
tyRet' _ _ _ (I.TyInteger _) _ = mkTIdent "Int"
tyRet' _ _ _ (I.TyFloat Short) _ = mkTIdent "Float"
tyRet' _ _ _ (I.TyFloat _) _ = mkTIdent "Double"
tyRet' _ _ ffi (I.TyApply (I.TySigned False) (I.TyInteger LongLong)) _ | ffi = mkTIdent "Double"
                                                                       | otherwise = mkTIdent "Word64"
tyRet' _ _ _ (I.TyApply (I.TySigned False) (I.TyInteger _)) _ = mkTIdent "Word"
tyRet' _ _ ffi (I.TyApply _ (I.TyInteger LongLong)) _ | ffi = mkTIdent "Double"
                                                      | otherwise = mkTIdent "Int64"
tyRet' _ _ _ (I.TyApply _ (I.TyInteger _)) _ = mkTIdent "Int"
tyRet' _ _ _ (I.TyObject) _ = mkTIdent "GObject"
tyRet' _ _ _ (I.TyAny) _ = mkTIdent "JSVal"
tyRet' pname enums True (I.TyPromise I.TyVoid) ext = mkTIdent "JSVal"
tyRet' pname enums True (I.TyPromise t) ext = H.HsTyTuple [mkTIdent "JSVal", tyRet' pname enums True t ext]
tyRet' pname enums False (I.TyPromise t) ext = tyRet' pname enums False t ext
tyRet' pname enums ffi (I.TySum t) ext = mkTIdent (sumType t)
tyRet' pname enums ffi (I.TyRecord (I.TyName a _) (I.TyName b _)) ext = H.HsTyApp (H.HsTyApp (mkTIdent "Record") (mkTIdent $ typeFor a)) (mkTIdent $ typeFor b)
--tyRet' _ enums True (I.TyOptional (I.TyInteger LongLong)) _ = H.HsTyApp (mkTIdent "Nullable") (mkTIdent "Double")
--tyRet' _ enums True (I.TyOptional (I.TyApply _ (I.TyInteger LongLong))) _ = H.HsTyApp (mkTIdent "Nullable") (mkTIdent "Double")
tyRet' _ enums True t ext = mkTIdent "JSVal"
tyRet' _ _ _ t _ = error $ "Return type " ++ show t

eventTyRet :: (String -> Bool) -> String -> String -> H.HsType
eventTyRet isLeaf interface eventName =
  H.HsTyApp
    (H.HsTyApp
      (mkTIdent "EventName")
      (tySelf isLeaf interface)
    )
    (mkTIdent $ eventType interface eventName)

-- The same, for a concrete type
--
--cnRet :: I.Type -> H.HsType
--
--cnRet (I.TyName c Nothing) = case asIs c of
--  Nothing -> mkTIdent ('T' : c)
--  Just c' -> mkTIdent c'
--cnRet z = tyRet z

-- Obtain a return type context (if any) from a return type

ctxSelf :: (String -> Bool) -> String -> [H.HsAsst]
ctxSelf isLeaf t | isLeaf t  = []
                 | otherwise = [(mkUIdent (classFor t),[mkTIdent "self"])]

tySelf :: (String -> Bool) -> String -> H.HsType
tySelf isLeaf t | isLeaf t  = mkTIdent (typeFor t)
                | otherwise = mkTIdent "self"

ffiTySelf intf = mkTIdent (typeFor $ getDef intf)

ctxRet :: WrapType -> I.Type -> [H.HsAsst]
ctxRet Underscore _ = []
ctxRet Unsafe t = (mkUIdent "HasCallStack", []) : ctxRet Normal t
ctxRet _ t = ctxRet' "result" t

ctxRet' :: String -> I.Type -> [H.HsAsst]

ctxRet' pname t | isStringType t = [(mkUIdent "FromJSString", [mkTIdent (paramName' pname)])]
--ctxRet' pname (I.TyName "XMLHttpRequestResponseType" Nothing) = [(mkUIdent "FromJSString", [mkTIdent (paramName pname)])]
--ctxRet (I.TyName c Nothing) = case (asIs False c) of
--  Nothing -> [(mkUIdent $ classFor c, [mkTIdent "self"])]
--  Just c' -> []
ctxRet' pname (I.TySequence t _) = ctxRet' pname t
ctxRet' pname (I.TySafeArray t) = ctxRet' pname t
ctxRet' pname (I.TyFrozenArray t) = ctxRet' pname t
ctxRet' pname (I.TyOptional t) = ctxRet' pname t
ctxRet' pname (I.TyPromise t) = ctxRet' pname t
ctxRet' pname _ = []

-- Obtain a type signature from a parameter definition

tyParm :: [String] -> (String -> Bool) -> I.Param -> (H.HsType, [H.HsAsst])
tyParm enums isLeaf = tyParm' enums isLeaf False

tyParmFFI :: [String] -> (String -> Bool) -> I.Param -> (H.HsType, [H.HsAsst])
tyParmFFI enums isLeaf = tyParm' enums isLeaf True

tyParm' :: [String] -> (String -> Bool) -> Bool -> I.Param -> (H.HsType, [H.HsAsst])
tyParm' enums isLeaf ffi param@(I.Param optional (I.Id _) ptype [I.Mode In] ext) =
  if optional == I.Optional && canBeMaybe ptype
    then
        case lookup ptype of
            r@(H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) _, _) -> r
            r@(H.HsTyApp (H.HsTyVar (H.HsIdent "Optional")) _, _) -> r
            r@(H.HsTyApp (H.HsTyCon (H.Special H.HsListCon)) _, _) -> r
            (hsType, hsAsst) | ffi -> (H.HsTyApp (mkTIdent "Optional") hsType, hsAsst)
                             | otherwise -> (H.HsTyApp (mkTIdent "Maybe") hsType, hsAsst)
    else lookup ptype
 where
  p = mkTIdent (paramName param)
  lookup ptype =
   case ptype of
    I.TyOptional t@(I.TySequence _ _) | ffi -> (mkTIdent "JSVal", [])
                                      | otherwise -> lookup t
    I.TyOptional (I.TyName c Nothing) | ffi && c `elem` enums ->
        (H.HsTyApp (mkTIdent "Optional") (mkTIdent c), [])
    I.TyName c Nothing | optional == I.Optional && ffi && c `elem` enums ->
        (H.HsTyApp (mkTIdent "Optional") (mkTIdent c), [])
    I.TyOptional t | ffi ->
        case lookup t of
            r@(H.HsTyApp (H.HsTyVar (H.HsIdent "Optional")) _, _) -> r
            (hsType, hsAsst) -> (H.HsTyApp (mkTIdent "Optional") hsType, hsAsst)
--        case lookup t of
--            (H.HsTyApp (H.HsTyVar (H.HsIdent "JSVal")) a, b) -> (H.HsTyApp (mkTIdent "JSVal")
--                                                                (H.HsTyApp (mkTIdent "Maybe") a), b)
--            (hsType, hsAsst) -> (H.HsTyApp (mkTIdent "Maybe") hsType, hsAsst)
--    I.TyOptional t | isStringType t -> lookup t
    I.TyOptional t | not ffi ->
        case lookup t of
            r@(H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) _, _) -> r
            (hsType, hsAsst) -> (H.HsTyApp (mkTIdent "Maybe") hsType, hsAsst)
    I.TySequence t _ | ffi -> (mkTIdent "JSVal", [])
                     | otherwise -> let (hsType, hsAsst) = lookup t in (mkTyList hsType, hsAsst)
    I.TySafeArray t | ffi -> (mkTIdent "JSVal", [])
                    | otherwise -> let (hsType, hsAsst) = lookup t in (mkTyList hsType, hsAsst)
    I.TyFrozenArray t | ffi -> (mkTIdent "JSVal", [])
                    | otherwise -> let (hsType, hsAsst) = lookup t in (mkTyList hsType, hsAsst)
    I.TyName c Nothing | c `elem` ["NotificationPermissionCallback", "StringCallback"] -> if
                                 | ffi -> (H.HsTyApp (mkTIdent "Optional")
                                        (H.HsTyApp (mkTIdent (typeFor c)) p), [])
                                 | otherwise -> (H.HsTyApp (mkTIdent "Maybe")
                                        (H.HsTyApp (mkTIdent (typeFor c)) p), [(mkUIdent "ToJSString", [p])])
    t | isStringType t && ffi -> (mkTIdent "JSString", [])
      | isStringType t -> (p, [(mkUIdent "ToJSString", [p])])
    I.TyName "DOMString..." Nothing | ffi -> (mkTIdent "JSVal", [])
                                    | otherwise -> (mkTIdent $ "[" ++ paramName param ++ "]", [(mkUIdent "ToJSString", [p]), (mkUIdent "ToJSVal", [p])])
    I.TyName "GLintptr" Nothing | ffi -> (mkTIdent "Double",[])
    I.TyName "GLsizeiptr" Nothing | ffi -> (mkTIdent "Double",[])
    I.TyName "GLint64" Nothing | ffi -> (mkTIdent "Double",[])
    I.TyName "GLuint64" Nothing | ffi -> (mkTIdent "Double",[])
    I.TyName c Nothing -> case asIs enums isLeaf ffi c of
      Just cc       -> (cc, [])
      Nothing | ffi -> (mkTIdent (typeFor c), [])
      _             -> (p, [(mkUIdent $ classFor c, [p])])
    I.TyInteger LongLong | ffi -> (mkTIdent "Double",[])
                         | otherwise -> (mkTIdent "Int64",[])
    I.TyInteger _ -> (mkTIdent "Int",[])
    I.TyFloat Short -> (mkTIdent "Float",[])
    I.TyFloat _ -> (mkTIdent "Double",[])
    I.TyApply (I.TySigned False) (I.TyInteger LongLong) | ffi -> (mkTIdent "Double",[])
                                                        | otherwise -> (mkTIdent "Word64",[])
    I.TyApply (I.TySigned False) (I.TyInteger _) -> (mkTIdent "Word",[])
    I.TyApply _ (I.TyInteger LongLong) | ffi -> (mkTIdent "Double",[])
                                       | otherwise -> (mkTIdent "Int64",[])
    I.TyApply _ (I.TyInteger _) -> (mkTIdent "Int",[])
    I.TyAny | ffi -> (mkTIdent "JSVal", [])
            | otherwise -> (p, [(mkUIdent "ToJSVal", [p])])
    I.TyPromise t -> lookup t
    I.TySum t -> let c = sumType t in case asIs enums isLeaf ffi c of
      Just cc       -> (cc, [])
      Nothing | ffi -> (mkTIdent (typeFor c), [])
      _             -> (p, [(mkUIdent $ classFor c, [p])])
    I.TyRecord (I.TyName a _) (I.TyName b _) -> (H.HsTyApp (H.HsTyApp (mkTIdent "Record") (mkTIdent $ typeFor a)) (mkTIdent $ typeFor b), [])
    _ | ffi -> let (hsType, hsAsst) = tyParm' enums isLeaf False param in (hsType, hsAsst)
    t -> error $ "Param type " ++ show t

tyParm' _ _ _ param@I.Param{} = error $ "Unsupported parameter attributes " ++ show param


