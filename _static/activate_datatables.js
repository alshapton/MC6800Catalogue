// Copyright (c) 2023 Varun Sharma
//
// SPDX-License-Identifier: MIT

$(document).ready( function () {
    $.extend( $.fn.dataTable.defaults,
        {
            "pageLength": -1,
            "language": {
                "lengthLabels": {
                    "-1": "Show all"
                },
                "search": "Search (on this page): "
            },
            "lengthMenu": [
                10,
                25,
                50,
                100,
                200,
                -1
            ]
        },
    );

    $(`table.sphinx-datatable`).filter(':not(.dataTable)').DataTable(
        {},
    );
} );