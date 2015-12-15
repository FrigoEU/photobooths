module App.GUI.Views.StatisticsPage where

import Prelude

import OpticUI.Markup.HTML as H
import OpticUI.Markup as H
import OpticUI.Components (textField)
import OpticUI (with, withView, traversal, ui, text, UI(), Markup(), runHandler, foreach, Handler())
import OpticUI.Components.Async (onResult, async)

import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Exception (message, Error())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (REF())
import Control.Apply ((*>))

import Data.Lens (lens, traversed, Lens(), set, over, view)
import Data.Lens.Common (_Just, _Nothing)
import Data.Foldable (mconcat, find)
import Data.Array (snoc, (!!), updateAt)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Monoid (mempty)
import Data.StrMap (StrMap(), empty, lookup)

import App.Model.Photobooth
import App.Model.Event
import App.Model.Statistic
import App.Model.Async
import App.Endpoint
import App.GUI.Types
import App.GUI.State
import App.GUI.Components.Exec

------------------------------------------

statisticsPage :: forall eff. 
                  String ->
                  AppUI (ref :: REF | eff) { statistics :: AsyncModel (ref :: REF | eff) AllStatistics 
                                           , events :: AsyncModel (ref :: REF | eff) (Array (EventWithState (ref :: REF | eff)))}
statisticsPage cn = ui (H.h1 [H.classA "page-title"] $ H.text ("Statistics for: " <> cn)) <> (with \s h -> mconcat [
     (_statistics <<< _Busy) $ onResult (\a -> runHandler h (set _statistics (Done a) s)) (\err -> runHandler h (set _statistics (Errored err) s)) <> (ui $ H.div [] $ H.text "Loading statistics"),
     (_statistics <<< _Errored) $ with \err _ -> ui $ H.div [H.classA "alert alert-danger"] $ H.text ("Statistics failed to load: " <> message err),
     (_events <<< _Busy) $ onResult (\a -> runHandler h (set _events (Done a) s)) (\err -> runHandler h (set _events (Errored err) s)) <> (ui $ H.div [] $ H.text "Loading events"),
     (_events <<< _Errored) $ with \err _ -> ui $ H.div [H.classA "alert alert-danger"] $ H.text ("Events failed to load: " <> message err)
  ]) <> with c
    where 
      c {statistics: (Done (AllStatistics {eventStatistics: es, monthlyStatistics: ms})), events: (Done eventsWithState)} _ = 
        ui $ (H.div [H.classA "crud-table-wrapper"] <<< H.table [H.classA "table crud-table"] <<< H.tbody [] <<< mconcat) 
          ([header] <> map monthlyStatisticsLine ms <> map (eventStatisticsLine eventsWithState) es)
      c _ _ = mempty

header :: Markup
header = H.tr [] $ mconcat [
    H.th [] $ H.text "Classificatie",
    H.th [] $ H.text "Fotos",
    H.th [] $ H.text "Prints"
  ]

monthlyStatisticsLine :: MonthlyStatistic -> Markup
monthlyStatisticsLine (MonthlyStatistic ms) = H.tr [] $ mconcat [
    H.td [] $ H.text (ms.computername <> " " <> getMonthText ms.month),
    H.td [] $ H.text $ show ms.pictures,
    H.td [] $ H.text $ show ms.prints
  ]

eventStatisticsLine :: forall eff. Array (EventWithState eff) -> EventStatistic -> Markup
eventStatisticsLine events (EventStatistic es) = H.tr [] $
  case find (\{model: Event ev} -> maybe false (\i -> i == es.eventId) ev.id) events of
       Nothing -> H.td [] $ H.text "No event found for statistic"
       Just {model: Event ev} -> mconcat [
           H.td [] $ H.text (ev.computername <> " " <> ev.name <> 
                                  ": Van " <> show ev.datefrom <> " tot " <> show ev.dateuntil),
           H.td [] $ H.text $ show es.pictures,
           H.td [] $ H.text $ show es.prints
         ]

