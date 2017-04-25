const should = require('should');
const express = require('express');
const Nightmare = require('nightmare');


const SHOW_BROWSER = !!Number(process.env['SHOW_BROWSER']);
const TYPE_INTERVAL = Number(process.env['TYPE_INTERVAL']);

function createServer() {
    const app = express();

    return server = app
        .use(express.static('dist'))
        .listen(3000);ц
}

describe('E-2-E testing', function () {
    const appUrl = 'http://localhost:3000';
    let server;
    let nightmare;

    this.timeout('10s');

    before(() => {
        server = createServer();
    });
    after(() => {
        server.close();
    });


    beforeEach(() => {
        nightmare = new Nightmare({
            show: SHOW_BROWSER,
            typeInterval: TYPE_INTERVAL || 100
        });
    });

    describe('/ (Elm App)', () => {
        it('should open elm-weather app page', (done) => {
            nightmare
                .goto(appUrl)
                .evaluate(() => document.title)
                .end()
                .then(title => {
                    title.should.be.equal('Elm App');
                    done();
                })
                .catch(done);
        });

        it('should render the page with correct form', (done) => {
            nightmare
                .goto(appUrl)
                .exists('.zip-input')
                .then(zipInputExist => {
                    zipInputExist.should.be.true();
                })
                .then(() => nightmare.exists('.button'))
                .then(buttonExists => {
                    buttonExists.should.be.true();
                })
                .then(() => nightmare.end())
                .then(done)
                .catch(done);
        });

        describe('using the app', () => {
            const zipCode = '90623';

            it('should fetch weather and show it', (done) => {
                nightmare
                    .goto(appUrl)
                    .type('.zip-input', zipCode)
                    .click('.button')
                    .wait(`#zip-${zipCode}`)
                    .end()
                    .then(done)
                    .catch(done);
            });

            it('should not add the same zip code twice', (done) => {
                nightmare
                    .goto(appUrl)
                    .type('.zip-input', zipCode)
                    .click('.button')
                    .wait(`#zip-${zipCode}`)
                    .type('.zip-input', zipCode)
                    .click('.button')
                    .wait(1000)
                    .evaluate(() => {
                        const locations = document.querySelectorAll('div.location');
                        return locations.length;
                    })
                    .end()
                    .then(locationCount => {
                        locationCount.should.be.equal(1);
                        done();
                    })
                    .catch(done);
            });

            it('should not add unexisted zips', (done) => {
                nightmare
                    .goto(appUrl)
                    .type('.zip-input', '00000')
                    .click('.button')
                    .wait(2000)
                    .exists('div.location')
                    .end()
                    .then((isLocationExists) => {
                        isLocationExists.should.be.false();
                        done();
                    })
                    .catch(done);
            });

            it.skip('should update the weather each INTERVAL', (done) => {
                // It would be great to have some mocks for HTTP query
                // like it can be done by `nock` package but it works only in node environment
                // Good idea is to have INTERVAl be configurable by env variables
                done();
            });
        });
    });
});

