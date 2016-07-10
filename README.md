# photobooths

This is an application that will soon run (on a tiny scale) in production. It's a CRUD application for (physical) photobooths. The photobooths have a default configuration setup (= defaultprofile). You can schedule events for each photobooth, for which you can select another profile and/or upload seperate files that overwrite configuration files in the profile.

Aspects:
- Written in PureScript
- Frontend and Backend in one language = "universal"
- Static typing stretching from front to back
- Using optic-ui on frontend
- SQLite Database

First: 
    npm install
    bower install

Test:
    npm run test

E2E testing:
    check WorkerTest.purs

Build dev:
    npm run dev

Build prod (needs JAVA, uses closure compiler):
    npm run start

