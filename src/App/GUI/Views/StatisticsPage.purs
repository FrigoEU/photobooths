module App.GUI.Views.StatisticsPage where

import Prelude (show, (<>), (<<<), (<$>), ($), (==), map)

import OpticUI.Markup.HTML (td, tr, classA, div, em, onClick, button) as H
import OpticUI.Markup (text) as H
import OpticUI (with, ui, Markup(), runHandler)
import OpticUI.Components.Async (onResult)

import Control.Monad.Eff.Exception (message)
import Control.Monad.Eff.Ref (REF())

import Data.Lens (set)
import Data.Foldable (mconcat, find)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)

import App.Model.Event (Event(Event))
import App.Model.Statistic (AllStatistics(AllStatistics), EventStatistic(EventStatistic), MonthlyStatistic(MonthlyStatistic), getMonthText)
import App.Model.Async (AsyncModel(Errored, Done), _Errored, _Busy)
import App.Model.Date (toLocalDatetime)
import App.GUI.Types (AppUI)
import App.GUI.State (Route(PhotoboothsPage), _events, _statistics)
import App.GUI.Components.Markup (pageTitle, tableHeader, crudTable)
import App.GUI.Router (Nav)

statisticsPage :: forall eff. 
                  String ->
                  Nav (ref :: REF | eff) ->
                  AppUI (ref :: REF | eff) { statistics :: AsyncModel (ref :: REF | eff) AllStatistics 
                                           , events :: AsyncModel (ref :: REF | eff) (Array Event)}
statisticsPage cn nav = ui (pageTitle $ H.button [ H.classA "btn-nav" , H.onClick \_ -> nav PhotoboothsPage] (H.text "^") <> H.text " Statistics for: " <> H.em [] (H.text cn)) 
  <> (with \s h -> mconcat [
     (_statistics <<< _Busy) $ onResult (\a -> runHandler h (set _statistics (Done a) s)) (\err -> runHandler h (set _statistics (Errored err) s)) <> (ui $ H.div [] $ H.text "Loading statistics"),
     (_statistics <<< _Errored) $ with \err _ -> ui $ H.div [H.classA "alert alert-danger"] $ H.text ("Statistics failed to load: " <> message err),
     (_events <<< _Busy) $ onResult (\a -> runHandler h (set _events (Done a) s)) (\err -> runHandler h (set _events (Errored err) s)) <> (ui $ H.div [] $ H.text "Loading events"),
     (_events <<< _Errored) $ with \err _ -> ui $ H.div [H.classA "alert alert-danger"] $ H.text ("Events failed to load: " <> message err)
  ]) <> with c
    where 
      c {statistics: (Done (AllStatistics {eventStatistics, monthlyStatistics})), events: (Done eventsWithState)} _ = 
        ui $ (crudTable <<< mconcat) ([tableHeader [] [ "Classificatie", "Fotos", "Prints" ]] 
                                   <> map monthlyStatisticsLine monthlyStatistics 
                                   <> map (eventStatisticsLine eventsWithState) eventStatistics)
      c _ _ = mempty

monthlyStatisticsLine :: MonthlyStatistic -> Markup
monthlyStatisticsLine (MonthlyStatistic ms) = H.tr [] <<< mconcat $ (H.td [] <<< H.text) <$> [
    (ms.computername <> " " <> getMonthText ms.month),
    show ms.pictures,
    show ms.prints
  ]

eventStatisticsLine :: Array Event -> EventStatistic -> Markup
eventStatisticsLine events (EventStatistic es) = H.tr [] $
  case find (\(Event ev) -> maybe false (\i -> i == es.eventId) ev.id) events of
       Nothing -> H.td [] $ H.text "No event found for statistic"
       Just (Event ev) -> mconcat $ (H.td [] <<< H.text) <$> [
           (ev.computername <> " " <> ev.name <> ": Van " <> toLocalDatetime ev.datefrom <> " tot " <> toLocalDatetime ev.dateuntil),
           show es.pictures,
           show es.prints
         ]

