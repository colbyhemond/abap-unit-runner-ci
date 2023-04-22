require('dotenv').config({path:'./.env.local'});

const axios = require('axios');
const fs = require('fs');

const root = './src/'

const isClass = file => {
    const regex = /clas.abap/
    return file.match(regex) ? true : false
}

const filterClassFiles = (fileList, root) => {
    let classFiles = []
    fileList.map((file, i) => {
        const pathAndFile = root + file
        if (fs.lstatSync(pathAndFile).isFile()) {
            isClass(file) ? classFiles.push(file) : null          
        } else {
            const data = fs.readdirSync(pathAndFile, 'utf8');
            const filesArr = filterClassFiles(data, `${pathAndFile}/`)
            filesArr.map((file) => {
                classFiles.push(file)
            })
        }
    })
    return classFiles
}

try {

    if (process.env.SAP_DEFAULT_HOST === undefined) {
        throw('env SAP_DEFAULT_HOST is undefined')
    }
    if (process.env.SAP_USERNAME === undefined) {
        throw('env SAP_USERNAME is undefined')
    }
    if (process.env.SAP_PASSWORD === undefined) {
        throw('env SAP_PASSWORD is undefined')
    }

    const data = fs.readdirSync(root, 'utf8');
    let classFiles = filterClassFiles(data, root)
    let classNames = []
    classFiles.map((file) => {
        classNames.push(file.split('.')[0])
    })
    classFiles = []
    if (classNames.length = 0) {
        //no classes to run unit tests on
        return
    }
    
    axios.post(`${process.env.SAP_DEFAULT_HOST}/abapunitrunner/ci/runner`, {
        class: classNames[0]
    },{
        auth: {
            username: process.env.SAP_USERNAME,
            password: process.env.SAP_PASSWORD
        }
    })
    .then((response) => {
        //implement response of unit test statuses
        console.log(response.status);
    })
    .catch((err) => {
        //implement better error handling
        console.log(err.status);
    })
    
    
    
} catch (err) {
    console.error(err);
}