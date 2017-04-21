const should = require('should');
const express = require('express');
const Nightmare = require('nightmare');


describe('E-2-E testing', function () {
    let server;
    let nightmare;

    this.timeout('10s');

    before(() => {
        const app = express();
        server = app
            .use(express.static('dist'))
            .listen(3000);
    });
    after(() => {
        server.close();
    });


    beforeEach(() => {
        nightmare = new Nightmare({ show: true });
    });

    describe('/', () => {
        it('should open elm-weather app page', (done) => {
            nightmare
                .goto('http://localhost:3000')
                .evaluate(() => document.title)
                .end()
                .then(title => {
                    title.should.be.equal('Elm App');
                    done();
                })
                .catch(done);
        });
    });
});

